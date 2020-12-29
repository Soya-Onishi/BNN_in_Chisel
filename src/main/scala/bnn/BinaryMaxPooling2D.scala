package bnn

import chisel3._
import chisel3.util._

class BinaryMaxPooling2D(
  kernelSize: (Int, Int),
  inputSize: Int,
  inputShape: (Int, Int, Int),
  stride: Int
) extends BinaryLayer {
  val (_, _, inputC) = inputShape

  val io = IO(new Bundle {
    val inData = Flipped(DecoupledIO(Pixel(Vec(inputSize, Bool()))))
    val outData = DecoupledIO(Pixel(Vec(inputC, Bool())))
    val isInit = Output(Bool())
  })

  val exec_pooling :: post_process :: Nil = Enum(2)
  val layerBase = Module(new Binary2DLayer(kernelSize, inputSize, inputShape, stride, Bool()))

  layerBase.io.inData <> io.inData
  layerBase.io.outData.ready := false.B
  io.outData.valid := false.B
  io.outData.bits  := DontCare
  io.isInit        := layerBase.io.isInit

  when(layerBase.io.outData.valid) {
    val pooling = VecInit(layerBase.io.outData.bits.map(_.bits)).reduceTree{
      case (left, right) => VecInit((left zip right).map{ case (l, r) => l | r })
    }

    val pixel = Wire(Pixel(Vec(inputC, Bool())))
    pixel.bits        := pooling
    pixel.left        := layerBase.io.isLeft
    pixel.right       := layerBase.io.isRight
    pixel.topLeft     := layerBase.io.isTopLeft
    pixel.bottomRight := layerBase.io.isBottomRight
    pixel.valid       := true.B

    io.outData.bits  := pixel
    io.outData.valid := true.B

    layerBase.io.outData.ready := true.B
  }

  /*
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
      globalState := wait_next
    }

    when(!window.io.window.valid & readyForNextPixel) {
      val reachBottomRight = nextPixelBits.foldLeft(false.B) { case (acc, pixel) => acc | pixel.bottomRight }
      shiftPolicy := Mux(reachBottomRight, force_shift, shiftPolicy)
      window.io.nextPixel.valid := true.B
      inputBufferIdx := 0.U
    }
  }
  */

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