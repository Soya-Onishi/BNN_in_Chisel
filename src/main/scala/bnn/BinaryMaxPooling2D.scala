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
}