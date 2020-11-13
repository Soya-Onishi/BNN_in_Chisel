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
  val windowW = math.ceil(kernelW.toFloat / stride.toFloat).toInt
  val windowH = kernelH * stride
  val weightsPerCycle = math.ceil(weights.length.toFloat / cyclesForAllWeights.toFloat).toInt
  val cyclesForWeights = math.ceil(weights.length.toFloat / weightsPerCycle.toFloat).toInt

  require(stride <= kernelH)
  require(stride <= kernelW)
  require(weights.length == bias.length)

  val io = IO(new Bundle {
    val inData = Flipped(Decoupled(Pixel(Vec(inputC, Bool()))))
    val outData = Decoupled(Pixel(Vec(weights.length, Bool())))
    val isInit = Output(Bool())
  })

  val init :: wait_executable :: execute_convolution :: Nil = Enum(3)
  val exec_conv :: post_process :: Nil = Enum(2)
  val wait_to_ready :: force_shift :: Nil = Enum(2)
  val globalState = RegInit(init)
  val runningState = RegInit(exec_conv)
  val shiftPolicy = RegInit(wait_to_ready)
  val sendBitsDone = RegInit(false.B)
  val sendNextPixelDone = RegInit(false.B)

  val bufferValid = RegInit(false.B)

  val inputBuffers = Reg(Vec(stride, Pixel(Vec(inputC, Bool()))))
  val inputBufferIdx = RegInit(0.U(requiredLength(stride).W))
  val isInputBufferFull = WireInit(inputBufferIdx === stride.U)
  val nextInputBufferIdx = WireInit(inputBufferIdx + 1.U)
  val reachBottomRight = RegInit(false.B)

  val window = Module(new WindowBuffer(Vec(inputC, Bool()), (inputH, inputW), kernelSize, stride))
  val weightIdx = RegInit(0.U(requiredLength(cyclesForAllWeights).W))

  val isBufferLeft        = RegInit(false.B)
  val isBufferRight       = RegInit(false.B)
  val isBufferTopLeft     = RegInit(false.B)
  val isBufferBottomRight = RegInit(false.B)
  val activatedBuffer     = Reg(Vec(cyclesForWeights, Vec(weightsPerCycle, Bool())))

  io.inData.ready             := !isInputBufferFull
  io.outData.valid            := bufferValid
  io.outData.bits.bits        := DontCare
  io.outData.bits.valid       := false.B
  io.outData.bits.left        := false.B
  io.outData.bits.right       := false.B
  io.outData.bits.topLeft     := false.B
  io.outData.bits.bottomRight := false.B
  io.isInit                   := globalState === init

  window.io.forceShift := false.B
  window.io.nextPixel.bits := Mux(
    io.inData.valid & nextInputBufferIdx === stride.U,
    VecInit(inputBuffers.init :+ io.inData.bits),
    inputBuffers
  )

  window.io.nextPixel.valid := false.B

  when(io.inData.valid & !isInputBufferFull & globalState =/= init) {
    inputBuffers(inputBufferIdx) := io.inData.bits
    inputBufferIdx := nextInputBufferIdx
  }

  switch(globalState) {
    is(init) {
      initState()
    }
    is(wait_executable) {
      waitToExecute()
    }
    is(execute_convolution) {
      convolutionState()
    }
  }

  private def initState(): Unit = {
    when(!window.io.isInit) {
      globalState := wait_executable
    }
  }

  private def waitToExecute(): Unit = {
    when(window.io.window.valid){
      shiftPolicy := wait_to_ready
      globalState := execute_convolution
    } .otherwise {
      val nextBufferFull = nextInputBufferIdx === stride.U
      val readyInput = io.inData.valid

      when(nextBufferFull & readyInput) {
        window.io.nextPixel.valid := true.B
        inputBufferIdx := 0.U
      }
    }
  }

  private def convolutionState(): Unit = {
    switch(runningState) {
      is(exec_conv) {
        convolution()

        val nextIdx = weightIdx + 1.U
        weightIdx := Mux(nextIdx === cyclesForWeights.U, 0.U, nextIdx)
        when(nextIdx === cyclesForWeights.U) {
          runningState := post_process
          sendBitsDone := false.B
          sendNextPixelDone := false.B
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

    val bitCounters = Seq.fill(weightsss.length)(Module(new BitCounter(inputC * kernelH * kernelW)))
    bitCounters.foreach(counter => counter.io.in := VecInit(Seq.fill(counter.width)(false.B)))

    when(window.io.window.valid) {
      (weightsss zip biasess).zipWithIndex.foreach {
        case ((weightss, biases), idx) =>
          when(weightIdx === idx.U) {
            val activatedBits = (weightss, biases, bitCounters).zipped.map {
              case (weights, bias, bitCounter) =>
                val bits = (weights zip window.io.window.bits).flatMap {
                  case (weight, pixels) => if(weight) pixels.bits else VecInit(Seq.fill(pixels.bits.length)(false.B))
                }

                bitCounter.io.in := VecInit(bits)
                bitCounter.io.count > bias.U
            }

            isBufferLeft        := window.io.isLeft
            isBufferRight       := window.io.isRight
            isBufferTopLeft     := window.io.isTopLeft
            isBufferBottomRight := window.io.isBottomRight
            activatedBuffer(idx.U) := VecInit(activatedBits)
          }
      }
    }

    val readyForNextPixel = ((nextInputBufferIdx === stride.U) & io.inData.valid) | isInputBufferFull
    when(!window.io.window.valid & readyForNextPixel) {
      window.io.nextPixel.valid := true.B
      inputBufferIdx := 0.U
    }
  }

  private def postProcess(): Unit = {
    val bits = VecInit(activatedBuffer.flatten)
    val pixel = Wire(Pixel(Vec(bits.length, Bool())))
    pixel.bits := bits
    pixel.valid := true.B
    pixel.left := isBufferLeft
    pixel.right := isBufferRight
    pixel.topLeft := isBufferTopLeft
    pixel.bottomRight := isBufferBottomRight

    when(io.outData.ready & !sendBitsDone) {
      io.outData.bits := pixel
      bufferValid := false.B
      sendBitsDone := true.B
    }

    val readyForNextPixel = ((nextInputBufferIdx === stride.U) & io.inData.valid) | isInputBufferFull
    when(shiftPolicy === wait_to_ready & readyForNextPixel & !sendNextPixelDone) {
      window.io.nextPixel.valid := true.B
      inputBufferIdx := 0.U
      sendNextPixelDone := true.B

      val reachBottomRight = inputBuffers.foldLeft(false.B) { case (acc, pixel) => acc | pixel.bottomRight }
      when(reachBottomRight) {
        shiftPolicy := force_shift
      }
    }

    when(shiftPolicy === force_shift & !sendNextPixelDone) {
      window.io.forceShift := true.B
      sendNextPixelDone := true.B

      val topLeftInBuffer = inputBuffers.foldLeft(false.B){ case (acc, pixel) => acc | pixel.topLeft }
      val transit = (inputBufferIdx =/= 0.U) | topLeftInBuffer
      when(transit) {
        shiftPolicy := wait_to_ready
      }
    }

    when(sendBitsDone & sendNextPixelDone) {
      runningState := exec_conv

      when(pixel.bottomRight) {
        globalState := wait_executable
      }
    }
  }
}