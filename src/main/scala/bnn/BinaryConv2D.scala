package bnn

import chisel3._
import chisel3.util.{Decoupled, Enum}
import chisel3.util.{switch, is, SwitchContext}

class BinaryConv2D(
  kernelSize: (Int, Int),
  weights: Seq[Seq[Boolean]],
  bias: Seq[Int],
  inputShape: (Int, Int, Int),
  cyclesForAllWeights: Int,
  stride: Int = 1,
) extends BinaryLayer {
  val (kernelH, kernelW) = kernelSize
  val (inputH, inputW, inputC) = inputShape
  require(stride <= kernelH)
  require(stride <= kernelW)
  require(weights.length == bias.length)

  val io = IO(new Bundle {
    val inData = Decoupled(BNNPixel(Vec(inputC, Bool())))
    val outData = Flipped(Decoupled(BNNPixel(Vec(weights.length, Bool()))))
    val isInit = Output(Bool())
  })

  val init :: running :: Nil = Enum(2)
  val exec_conv :: post_process :: wait_input :: Nil = Enum(3)
  val state = RegInit(init)
  val runningState = RegInit(exec_conv)

  val initWindowBNNPixel = Wire(BNNPixel(Vec(inputC, Bool())))
  initWindowBNNPixel.data         := VecInit(Seq.fill(inputC)(false.B))
  initWindowBNNPixel.isValid      := false.B
  initWindowBNNPixel.isLeftEdge   := false.B
  initWindowBNNPixel.isRightEdge  := false.B
  initWindowBNNPixel.isTopEdge    := false.B
  initWindowBNNPixel.isBottomEdge := false.B

  val windowH = kernelH * stride
  val windowW = math.ceil(kernelW.toFloat / stride.toFloat).toInt
  val window = RegInit(
    VecInit(Seq.fill(windowH)(
      VecInit(Seq.fill(windowW)(initWindowBNNPixel)))
    )
  )

  val memDepth = math.ceil((inputW - kernelW).toFloat / stride.toFloat).toInt
  val buffers = Vector.fill(windowH)(SyncReadMem(memDepth, BNNPixel(Vec(inputC, Bool()))))
  val memIdx = RegInit(0.U(requiredLength(memDepth).W))

  val weightsPerCycle = math.ceil(weights.length.toFloat / cyclesForAllWeights.toFloat).toInt
  val weightIdx = RegInit(0.U)
  val activatedBuffer = RegInit(Vec(cyclesForAllWeights, Vec(weightsPerCycle, Bool())))
  val bufferValid = RegInit(false.B)

  val inputBuffers = RegInit(Vec(stride, BNNPixel(Vec(inputC, Bool()))))
  val inputBufferIdx = RegInit(0.U)
  val isInputBufferFull = WireInit(inputBufferIdx === stride.U)
  val nextInputBufferIdx = WireInit(inputBufferIdx + 1.U)
  val readyToStride = isInputBufferFull | ((nextInputBufferIdx === stride.U) & io.inData.valid)
  val isStrideDone = RegInit(false.B)

  io.inData.ready := !isInputBufferFull
  io.outData.valid := bufferValid
  io.isInit := state === init

  when(io.inData.valid & !isInputBufferFull) {
    inputBuffers(inputBufferIdx) := io.inData.bits
    inputBufferIdx := nextInputBufferIdx
  }

  switch(state) {
    is(init) {
      initState()
    }
    is(running) {
      convolutionState()
    }
  }

  def initState(): Unit = {
    val nextIdx = WireInit(memIdx + 1.U)
    memIdx := Mux(nextIdx === memDepth.U, 0.U, nextIdx)
    when(nextIdx === memDepth.U) {
      state := running
    }

    val initValue = Wire(BNNPixel(Vec(inputC, Bool())))
    initValue.data := VecInit(Seq.fill(inputC)(false.B))
    initValue.isValid := false.B
    initValue.isLeftEdge := false.B
    initValue.isRightEdge := false.B
    initValue.isTopEdge := false.B
    initValue.isBottomEdge := false.B

    buffers.foreach(buf => buf.write(memIdx, initValue))
  }

  def convolutionState(): Unit = {
    switch(runningState) {
      is(exec_conv) {
        convolution()

        val nextIdx = weightIdx + 1.U
        val maxCount = if(weightsPerCycle == 1) weights.length else cyclesForAllWeights
        weightIdx := Mux(nextIdx === maxCount.U, 0.U, nextIdx)
        when(nextIdx === maxCount.U) {
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

    (weightsss zip biasess).zipWithIndex.foreach {
      case ((weightss, biases), idx) =>
        when(weightIdx === idx.U) {
          val activatedBits = (weightss, biases, bitCounters).zipped.map {
            case (weights, bias, bitCounter) =>
              val bits = (weights zip formattedWindow.data).flatMap { case (weight, pixels) => Mux(weight.B, pixels, Vec(pixels.length, false.B)) }
              bitCounter.io.in := VecInit(bits)

              bitCounter.io.count > bias.U
          }

          activatedBuffer(idx.U) := activatedBits
        }
    }
  }

  private def postProcess(): Unit = {
    when(io.outData.ready) {
      io.outData.bits := VecInit(activatedBuffer.flatten)
      bufferValid := false.B
    }

    when(readyToStride & !isStrideDone) {
      val partitioned = window.sliding(stride, stride).toSeq
      val readIdx = memIdx + 1.U
      val lasts = buffers.map(_.read(readIdx))
      val tails = partitioned.flatMap(_.map(_.tail))
      val nextWindow = (tails zip lasts).map{ case (row, l) => row :+ l }.map(row => VecInit(row))
      window := VecInit(nextWindow)

      val heads = partitioned.map(_.map(_.head)).tail.flatten
      val writeIdx = memIdx
      (heads zip buffers).foreach{ case (head, buffer) => buffer.write(writeIdx, head) }
      val remains = buffers.length - heads.length
      (buffers.takeRight(remains) zip inputBuffers).foreach{ case (buffer, pixel) => buffer.write(writeIdx, pixel) }

      isStrideDone := true.B
    }

    val transitToConv = (io.outData.ready | !bufferValid) & (readyToStride | isStrideDone)
    when(transitToConv) {
      runningState := exec_conv
      isStrideDone := false.B
      inputBufferIdx := 0.U
    }
  }

  private def formatWindow(kernelW: Int, windowW: Int, stride: Int): BNNPixel[Vec[Vec[Bool]]] = {
    val row = windowW * stride

    val windowData = window.flatten.zipWithIndex.collect { case (data, idx) if idx % row < kernelW => data }
    val isValid = windowData.foldLeft(true.B) { case (acc, pixel) => acc & pixel.isValid }
    val isLeftEdge = windowData.foldLeft(false.B) { case (acc, pixel) => acc | pixel.isLeftEdge }
    val isRightEdge = windowData.foldLeft(false.B) { case (acc, pixel) => acc | pixel.isRightEdge }
    val isTopEdge = windowData.foldLeft(false.B) { case (acc, pixel) => acc | pixel.isTopEdge }
    val isBottomEdge = windowData.foldLeft(false.B) { case (acc, pixel) => acc | pixel.isBottomEdge }
    val data = windowData.map(_.data)

    val formattedWindow = Wire(BNNPixel(Vec(kernelH * kernelW, Vec(inputC, Bool()))))
    formattedWindow.data := VecInit(data)
    formattedWindow.isValid := isValid
    formattedWindow.isLeftEdge := isLeftEdge
    formattedWindow.isRightEdge := isRightEdge
    formattedWindow.isTopEdge := isTopEdge
    formattedWindow.isBottomEdge := isBottomEdge

    formattedWindow
  }
}