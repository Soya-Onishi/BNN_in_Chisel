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
  val runningState = RegInit(exec_pooling)

  val nextPixelSendDone = RegInit(false.B)
  val outPixelSending = RegInit(false.B)
  val outPixelBuffer  = Reg(Pixel(Vec(inputC, Bool())))

  io.outData.valid := outPixelSending
  io.outData.bits  := outPixelBuffer

  val readyForNextPixel = ((nextInputBufferIdx === stride.U) & io.inData.valid) | isInputBufferFull

  compile()

  protected override def executionState(): Unit = {
    switch(runningState) {
      is(exec_pooling) {
        executePooling()
      }
      is(post_process) {
        postProcess()
      }
    }
  }

  private def executePooling(): Unit = {
    when(window.io.window.valid) {
      val bits = window.io.window.bits.foldLeft(0.U(inputC.W)){ case (acc, pixel) => acc | pixel.bits.asUInt() }
      val applied = VecInit(bits.asBools())

      outPixelBuffer.left        := window.io.isLeft
      outPixelBuffer.right       := window.io.isRight
      outPixelBuffer.topLeft     := window.io.isTopLeft
      outPixelBuffer.bottomRight := window.io.isBottomRight
      outPixelBuffer.bits := applied
      outPixelSending := true.B
      nextPixelSendDone := false.B

      runningState := post_process
    }

    when(!window.io.window.valid & readyForNextPixel) {
      val reachBottomRight = nextPixelBits.foldLeft(false.B) { case (acc, pixel) => acc | pixel.bottomRight }
      shiftPolicy := Mux(reachBottomRight, force_shift, shiftPolicy)
      window.io.nextPixel.valid := true.B
      inputBufferIdx := 0.U
    }
  }

  private def postProcess(): Unit = {
    when(io.outData.ready) {
      outPixelSending := false.B
    }

    when(shiftPolicy === wait_to_ready & readyForNextPixel & !nextPixelSendDone) {
      window.io.nextPixel.valid := true.B
      inputBufferIdx := 0.U
      nextPixelSendDone := true.B

      val reachBottomRight = nextPixelBits.foldLeft(false.B) { case (acc, pixel) => acc | pixel.bottomRight }
      when(reachBottomRight) {
        shiftPolicy := force_shift
      }
    }

    when(shiftPolicy === force_shift & !nextPixelSendDone) {
      window.io.forceShift := true.B
      nextPixelSendDone := true.B

      val topLeftInBuffer = nextPixelBits.foldLeft(false.B){ case (acc, pixel) => acc | pixel.topLeft }
      val transit = (inputBufferIdx =/= 0.U) | topLeftInBuffer
      when(transit) {
        shiftPolicy := wait_to_ready
      }
    }

    when(!outPixelSending & nextPixelSendDone) {
      runningState := exec_pooling
      when(outPixelBuffer.bottomRight) {
        globalState := wait_executable
      }
    }
  }
}