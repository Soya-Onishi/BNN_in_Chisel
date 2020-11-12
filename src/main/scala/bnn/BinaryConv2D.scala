package bnn

import chisel3._
import chisel3.util.{Decoupled, Enum, Valid}
import chisel3.util.{SwitchContext, is, switch}

class BinaryConv2D(
  kernelSize: (Int, Int),
  weights: Seq[Seq[Boolean]],
  bias: Seq[Int],
  inputShape: (Int, Int, Int),
  cyclesForAllWeights: Int,
  stride: Int
) extends BinaryLayer {
  val (kernelH, kernelW) = kernelSize
  val (inputH, inputW, inputC) = inputShape
  val xstrideMax = inputW / stride
  val ystrideMax = inputH / stride

  require(stride <= kernelH)
  require(stride <= kernelW)
  require(weights.length == bias.length)

  val io = IO(new Bundle {
    val inData = Flipped(Decoupled(Valid(Vec(inputC, Bool()))))
    val outData = Decoupled(Vec(weights.length, Bool()))
    val isInit = Output(Bool())
  })

  val init :: wait_to_fill :: vertical_stride :: ready_to_next_frame :: execute_convolution :: Nil = Enum(5)
  val exec_conv :: post_process :: wait_input :: Nil = Enum(3)
  val right_to_left :: erase_row :: Nil = Enum(2)
  val globalState = RegInit(init)
  val runningState = RegInit(exec_conv)
  val verticalStrideState = RegInit(right_to_left)

  val initBNNPixel = Wire(Valid(Vec(inputC, Bool())))
  initBNNPixel.bits  := VecInit(Seq.fill(inputC)(false.B))
  initBNNPixel.valid := false.B

  val windowH = kernelH * stride
  val windowW = math.ceil(kernelW.toFloat / stride.toFloat).toInt
  val windowX = RegInit(0.U(requiredLength(xstrideMax).W))
  val windowY = RegInit(0.U(requiredLength(ystrideMax).W))
  val horizontalShiftCount = RegInit(0.U)
  val verticalShiftCount = RegInit(0.U)
  val window = RegInit(
    VecInit(Seq.fill(windowH)(
      VecInit(Seq.fill(windowW)(initBNNPixel)))
    )
  )

  val memDepth = math.ceil((inputW - kernelW).toFloat / stride.toFloat).toInt
  val buffers = Vector.fill(windowH)(SyncReadMem(memDepth, Valid(Vec(inputC, Bool()))))
  val memIdx = RegInit(0.U(requiredLength(memDepth).W))
  val readIdx = Wire(UInt())
  val writeIdx = Wire(UInt())
  readIdx := 0.U
  writeIdx := 0.U

  val weightsPerCycle = math.ceil(weights.length.toFloat / cyclesForAllWeights.toFloat).toInt
  val maxWeightIdxCount = if(weightsPerCycle == 1) weights.length else cyclesForAllWeights
  val weightIdx = RegInit(0.U(requiredLength(maxWeightIdxCount).W))
  val activatedBuffer = RegInit(
    VecInit(Seq.fill(cyclesForAllWeights)(
      VecInit(Seq.fill(weightsPerCycle)(false.B))
    ))
  )
  val bufferValid = RegInit(false.B)

  val inputBuffers = RegInit(VecInit(Seq.fill(stride)(initBNNPixel)))
  val inputBufferIdx = RegInit(0.U(requiredLength(stride).W))
  val isInputBufferFull = WireInit(inputBufferIdx === stride.U)
  val nextInputBufferIdx = WireInit(inputBufferIdx + 1.U)
  val readyToStride = isInputBufferFull | ((nextInputBufferIdx === stride.U) & io.inData.valid)
  val isStrideDone = RegInit(false.B)
  val requireStride = WireInit(false.B)

  io.inData.ready := !isInputBufferFull
  io.outData.valid := bufferValid
  io.outData.bits := VecInit(Seq.fill(weights.length)(false.B))
  io.isInit := globalState === init

  when(io.inData.valid & !isInputBufferFull) {
    inputBuffers(inputBufferIdx) := io.inData.bits
    inputBufferIdx := nextInputBufferIdx
  }

  switch(globalState) {
    is(init) {
      initState()
    }
    is(wait_to_fill) {
      waitToFill()
    }
    is(vertical_stride) {
      verticalStride()
    }
    is(ready_to_next_frame) {
      readyToNextFrame()
    }
    is(execute_convolution) {
      convolutionState()
    }
  }

  when(requireStride) {
    val nextIdx = memIdx + 1.U
    memIdx := Mux(nextIdx === memDepth.U, 0.U, nextIdx)
    readIdx := Mux(nextIdx === memDepth.U, 0.U, nextIdx)
    writeIdx := memIdx

    val partitioned = window.sliding(stride, stride).toSeq

    val lasts = buffers.map(_.read(readIdx))
    val tails = partitioned.flatMap(_.map(_.tail))
    val nextWindow = (tails zip lasts).map{ case (row, l) => row :+ l }.map(row => VecInit(row))
    window := VecInit(nextWindow)

    val heads = WireInit(VecInit(partitioned.map(_.map(_.head)).tail.flatten))
    (heads zip buffers).foreach{ case (head, buffer) => buffer.write(writeIdx, head) }
    val remains = buffers.length - heads.length
    val inputs = Mux(isInputBufferFull, inputBuffers, VecInit(inputBuffers.init :+ io.inData.bits))
    (buffers.takeRight(remains) zip inputs).foreach{ case (buffer, pixel) => buffer.write(writeIdx, pixel) }
  }

  def initState(): Unit = {
    val nextIdx = WireInit(memIdx + 1.U)
    memIdx := Mux(nextIdx === memDepth.U, 0.U, nextIdx)
    when(nextIdx === memDepth.U) {
      globalState := wait_to_fill
    }

    val initValue = Wire(Valid(Vec(inputC, Bool())))
    initValue.bits := VecInit(Seq.fill(inputC)(false.B))
    initValue.valid := false.B

    buffers.foreach(buf => buf.write(memIdx, initValue))
  }

  def waitToFill(): Unit = {
    when(readyToStride) {
      requireStride := true.B
      inputBufferIdx := 0.U

      when(window(0)(1).valid) {
        globalState := execute_convolution
        windowX := 0.U
        windowY := 0.U
      }
    }
  }

  def verticalStride(): Unit = {
    val nextH = horizontalShiftCount + 1.U
    val nextV = verticalShiftCount + 1.U
    val maxV = Mux(windowY === ystrideMax.U, (inputH % stride).U, stride.U)

    requireStride := readyToStride
    when(readyToStride) {
      switch(verticalStrideState) {
        is(right_to_left) {
          horizontalShiftCount := Mux(nextH === windowW.U, 0.U, nextH)
          when(nextH === windowW.U) {
            val surpassThreshold = WireInit(verticalShiftCount === maxV)
            verticalShiftCount := Mux(surpassThreshold, 0.U, nextV)
            verticalStrideState := Mux(surpassThreshold, right_to_left, erase_row)
            when(surpassThreshold) {
              globalState := Mux(windowY === ystrideMax.U, ready_to_next_frame, execute_convolution)
            }
          }
        }
        is(erase_row) {
          horizontalShiftCount := Mux(nextH === xstrideMax.U, 0.U, nextH)
          when(nextH === xstrideMax.U) {
            verticalStrideState := right_to_left
          }
        }
      }
    }
  }

  def readyToNextFrame(): Unit = {
    val nextV = verticalShiftCount + 1.U
    val nextH = horizontalShiftCount + 1.U
    val shiftHMax = xstrideMax + windowW
    val shiftVMax = kernelH - 1

    when(readyToStride) {
      requireStride := true.B
      horizontalShiftCount := Mux(nextH === shiftHMax.U, 0.U, nextH)

      when(nextH === shiftHMax.U) {
        verticalShiftCount := Mux(nextV === shiftVMax.U, 0.U, nextV)
      }

      when((nextH === shiftHMax.U) & (nextV === shiftVMax.U)) {
        globalState := execute_convolution
        windowX := 0.U
        windowY := 0.U
      }
    }
  }

  def convolutionState(): Unit = {
    switch(runningState) {
      is(exec_conv) {
        convolution()

        val nextIdx = weightIdx + 1.U
        weightIdx := Mux(nextIdx === maxWeightIdxCount.U, 0.U, nextIdx)
        when(nextIdx === maxWeightIdxCount.U) {
          runningState := post_process
          bufferValid := true.B
        }
      }
      is(post_process) {
        postProcess()
      }
    }
  }

  private def convolution(): Unit = {
    val weightsss = weights.sliding(weightsPerCycle, weightsPerCycle).toSeq
    val biasess = bias.sliding(weightsPerCycle, weightsPerCycle).toSeq

    val formattedWindow = WireInit(formatWindow(kernelW, windowW, stride))
    val bitCounters = Seq.fill(weightsss.length)(Module(new BitCounter(inputC * kernelH * kernelW)))
    bitCounters.foreach(counter => counter.io.in := VecInit(Seq.fill(counter.width)(false.B)))

    (weightsss zip biasess).zipWithIndex.foreach {
      case ((weightss, biases), idx) =>
        when(weightIdx === idx.U) {
          val activatedBits = (weightss, biases, bitCounters).zipped.map {
            case (weights, bias, bitCounter) =>
              val bits = (weights zip formattedWindow.bits).flatMap { case (weight, pixels) => Mux(weight.B, pixels, VecInit(Seq.fill(pixels.length)(false.B))) }
              bitCounter.io.in := VecInit(bits)

              bitCounter.io.count > bias.U
          }

          activatedBuffer(idx.U) := activatedBits
        }
    }
  }

  private def postProcess(): Unit = {
    val nextX = windowX + 1.U
    val nextY = windowY + 1.U

    when(io.outData.ready) {
      io.outData.bits := VecInit(activatedBuffer.flatten)
      bufferValid := false.B
    }

    when(readyToStride & !isStrideDone) {
      requireStride := true.B
      isStrideDone := true.B
    }

    val transitNextState = (io.outData.ready | !bufferValid) & (readyToStride | isStrideDone)
    when(transitNextState) {
      windowX := Mux(nextX === xstrideMax.U, 0.U, windowX)
      windowY := Mux(nextX === xstrideMax.U, nextY, windowY)
      globalState := Mux(nextX === xstrideMax.U, vertical_stride, execute_convolution)
      horizontalShiftCount := 0.U
      verticalShiftCount := 0.U

      runningState := exec_conv
      isStrideDone := false.B
      inputBufferIdx := 0.U
    }
  }

  private def formatWindow(kernelW: Int, windowW: Int, stride: Int): Valid[Vec[Vec[Bool]]] = {
    val row = windowW * stride

    val windowData = window.flatten.zipWithIndex.collect { case (data, idx) if idx % row < kernelW => data }
    val isValid = windowData.foldLeft(true.B) { case (acc, pixel) => acc & pixel.valid }
    val data = windowData.map(_.bits)

    val formattedWindow = Wire(Valid(Vec(kernelH * kernelW, Vec(inputC, Bool()))))
    formattedWindow.bits  := VecInit(data)
    formattedWindow.valid := isValid

    formattedWindow
  }
}