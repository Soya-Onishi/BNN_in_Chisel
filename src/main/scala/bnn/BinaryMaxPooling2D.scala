package bnn

import chisel3._
import chisel3.util._

class BinaryMaxPooling2D(
  kernelSize: (Int, Int),
  inputSize: (Int, Int, Int),
  stride: Int
) extends Binary2DLayer[Bool, Bool](
  kernelSize,
  inputSize,
  inputSize._3,
  stride,
  Bool(),
  Bool()
) {
  val exec_pooling :: post_process :: Nil = Enum(2)

  val readyForNextPixel = ((nextInputBufferIdx === stride.U) & io.inData.valid) | isInputBufferFull

  compile()

  protected override def executionState(): Unit = {
    val bits = window.io.window.bits.foldLeft(0.U(inputC.W)){ case (acc, pixel) => acc | pixel.bits.asUInt() }
    val applied = VecInit(bits.asBools())

    io.outData.valid            := window.io.window.valid
    io.outData.bits.valid       := true.B
    io.outData.bits.left        := window.io.isLeft
    io.outData.bits.right       := window.io.isRight
    io.outData.bits.topLeft     := window.io.isTopLeft
    io.outData.bits.bottomRight := window.io.isBottomRight
    io.outData.bits.bits        := applied

    isBottomRight               := window.io.isBottomRight

    when(window.io.window.valid) {
      globalState := wait_to_next
    }

    when(!window.io.window.valid & readyForNextPixel) {
      val reachBottomRight = nextPixelBits.foldLeft(false.B) { case (acc, pixel) => acc | pixel.bottomRight }
      shiftPolicy := Mux(reachBottomRight, force_shift, shiftPolicy)
      window.io.nextPixel.valid := true.B
      inputBufferIdx := 0.U
    }
  }

//  private def postProcess(): Unit = {
//    when(io.outData.ready) {
//      outPixelSending := false.B
//    }
//
//    when(shiftPolicy === wait_to_ready & readyForNextPixel & !nextPixelSendDone) {
//      window.io.nextPixel.valid := true.B
//      inputBufferIdx := 0.U
//      nextPixelSendDone := true.B
//
//      val reachBottomRight = nextPixelBits.foldLeft(false.B) { case (acc, pixel) => acc | pixel.bottomRight }
//      when(reachBottomRight) {
//        shiftPolicy := force_shift
//      }
//    }
//
//    when(shiftPolicy === force_shift & !nextPixelSendDone) {
//      window.io.forceShift := true.B
//      nextPixelSendDone := true.B
//
//      val topLeftInBuffer = nextPixelBits.foldLeft(false.B){ case (acc, pixel) => acc | pixel.topLeft }
//      val transit = (inputBufferIdx =/= 0.U) | topLeftInBuffer
//      when(transit) {
//        shiftPolicy := wait_to_ready
//      }
//    }
//
//    when(!outPixelSending & nextPixelSendDone) {
//      runningState := exec_pooling
//      when(outPixelBuffer.bottomRight) {
//        globalState := wait_executable
//      }
//    }
//  }
}