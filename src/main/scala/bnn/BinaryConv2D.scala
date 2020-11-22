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
) extends Binary2DLayer(
  kernelSize,
  inputShape,
  weights.length,
  stride
) {
  val weightsPerCycle = math.ceil(weights.length.toFloat / cyclesForAllWeights.toFloat).toInt
  val cyclesForWeights = math.ceil(weights.length.toFloat / weightsPerCycle.toFloat).toInt

  require(weights.length == bias.length)

  val weightIdx = RegInit(0.U(requiredLength(cyclesForAllWeights).W))
  val activatedBuffer = Reg(Vec(cyclesForWeights, Vec(weightsPerCycle, Bool())))

  val exec_conv :: post_process :: Nil = Enum(2)
  val runningState = RegInit(exec_conv)
  val sendBitsDone = RegInit(false.B)
  val sendNextPixelDone = RegInit(false.B)

  val isBufferLeft = RegInit(false.B)
  val isBufferRight = RegInit(false.B)
  val isBufferTopLeft = RegInit(false.B)
  val isBufferBottomRight = RegInit(false.B)

  compile()

  override protected def executionState(): Unit = {
    switch(runningState) {
      is(exec_conv) {
        convolution()
      }
      is(post_process) {
        postProcess()
      }
    }
  }

  private def convolution(): Unit = {
    val weightsss = weights.sliding(weightsPerCycle, weightsPerCycle).toSeq
    val biasess = bias.sliding(weightsPerCycle, weightsPerCycle).toSeq

    val bitCounters = Seq.fill(weightsPerCycle)(Module(new BitCounter(inputC * kernelH * kernelW)))
    bitCounters.foreach(counter => counter.io.in := VecInit(Seq.fill(counter.width)(false.B)))

    when(window.io.window.valid) {
      (weightsss zip biasess).zipWithIndex.foreach {
        case ((weightss, biases), idx) =>
          when(weightIdx === idx.U) {
            val activatedBits = (weightss, biases, bitCounters).zipped.map {
              case (weights, bias, bitCounter) =>
                val bits = (weights zip window.io.window.bits).flatMap {
                  case (weight, pixels) => pixels.bits.map(_ ^ weight.asBool())
                }

                bitCounter.io.in := VecInit(bits)
                bitCounter.io.count > bias.U
            }

            isBufferLeft := window.io.isLeft
            isBufferRight := window.io.isRight
            isBufferTopLeft := window.io.isTopLeft
            isBufferBottomRight := window.io.isBottomRight
            activatedBuffer(idx.U) := VecInit(activatedBits)
          }
      }

      val nextIdx = weightIdx + 1.U
      weightIdx := Mux(nextIdx === cyclesForWeights.U, 0.U, nextIdx)
      when(nextIdx === cyclesForWeights.U) {
        runningState := post_process
        sendBitsDone := false.B
        sendNextPixelDone := false.B
      }
    }

    val readyForNextPixel = ((nextInputBufferIdx === stride.U) & io.inData.valid) | isInputBufferFull
    when(!window.io.window.valid & readyForNextPixel) {
      val reachBottomRight = nextPixelBits.foldLeft(false.B) { case (acc, pixel) => acc | pixel.bottomRight }
      shiftPolicy := Mux(reachBottomRight, force_shift, shiftPolicy)
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
      io.outData.valid := true.B
      sendBitsDone := true.B
    }

    val readyForNextPixel = ((nextInputBufferIdx === stride.U) & io.inData.valid) | isInputBufferFull
    when(shiftPolicy === wait_to_ready & readyForNextPixel & !sendNextPixelDone) {
      window.io.nextPixel.valid := true.B
      inputBufferIdx := 0.U
      sendNextPixelDone := true.B

      val reachBottomRight = nextPixelBits.foldLeft(false.B) { case (acc, pixel) => acc | pixel.bottomRight }
      shiftPolicy := Mux(reachBottomRight, force_shift, shiftPolicy)
    }

    when(shiftPolicy === force_shift & !sendNextPixelDone) {
      window.io.forceShift := true.B
      sendNextPixelDone := true.B

      val topLeftInBuffer = inputBuffers.foldLeft(false.B) { case (acc, pixel) => acc | pixel.topLeft }
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
