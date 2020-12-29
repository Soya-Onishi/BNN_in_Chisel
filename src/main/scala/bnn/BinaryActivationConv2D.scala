package bnn

import chisel3._
import chisel3.util._

class BinaryActivationConv2D(
  val size: Int,
  val inputWidth: Int,
  val biases: Seq[Int]
) extends BinaryLayer {
  val io = IO(new Bundle {
    val inData = Flipped(DecoupledIO(Pixel(Vec(size, UInt(inputWidth.W)))))
    val outData = DecoupledIO(Pixel(Vec(size, Bool())))
  })

  val inValid = RegInit(false.B)
  val inBits = Reg(Pixel(Vec(size, UInt(inputWidth.W))))

  val outValid = RegInit(false.B)
  val outBits = Reg(Pixel(Vec(size, Bool())))

  val activation = Module(new BinaryActivation2(size, inputWidth, biases))

  // ready signal is used here to determine whether renew registers
  // and this signal is passed to previous layer(i.e. BinaryConv2D)
  io.inData.ready := io.outData.ready
  when(io.outData.ready) {
    inBits  := io.inData.bits
    inValid := io.inData.valid

    outBits.bits        := activation.io.outData.bits
    outBits.topLeft     := inBits.topLeft
    outBits.bottomRight := inBits.bottomRight
    outBits.left        := inBits.left
    outBits.right       := inBits.right
    outBits.valid       := inBits.valid
    outValid            := activation.io.outData.valid
  }

  // From source
  activation.io.inData.valid := inValid
  activation.io.inData.bits  := inBits.bits

  activation.io.outData.ready := io.outData.ready
  io.outData.valid := outValid
  io.outData.bits  := outBits
}


/*
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
*/