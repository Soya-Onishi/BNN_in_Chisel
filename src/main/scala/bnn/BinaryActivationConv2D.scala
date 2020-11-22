package bnn

import chisel3._

class BinaryActivationConv2D(
  val channels: Int,
  val inputSize: Int,
  val inputWidth: Int,
  val biases: Seq[Int]
) extends BinaryActivation[Pixel[Vec[UInt]], Pixel[Vec[Bool]]](
  channels,
  inputSize,
  inputWidth,
  biases,
  Pixel(Vec(inputSize, UInt(inputWidth.W))),
  Pixel(Vec(channels, Bool()))
) {
  val isLeft        = Reg(Bool())
  val isRight       = Reg(Bool())
  val isTopLeft     = Reg(Bool())
  val isBottomRight = Reg(Bool())

  io.inData.ready             := globalState =/= sending
  io.outData.valid            := false.B
  io.outData.bits.bits        := VecInit(outputBuffer.flatten.take(channels))
  io.outData.bits.left        := isLeft
  io.outData.bits.right       := isRight
  io.outData.bits.topLeft     := isTopLeft
  io.outData.bits.bottomRight := isBottomRight
  io.outData.bits.valid       := true.B

  elaborate()

  override protected def renewBuffer(): Unit = {
    when(io.inData.valid & globalState =/= sending) {
      inputBuffer.bits  := io.inData.bits.bits

      when(nextIdx === 0.U) {
        isLeft        := io.inData.bits.left
        isRight       := io.inData.bits.right
        isTopLeft     := io.inData.bits.topLeft
        isBottomRight := io.inData.bits.bottomRight
      }
    }
  }
}
