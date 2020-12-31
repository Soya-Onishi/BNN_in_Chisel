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

  val activation = Module(new BinaryActivation[Pixel[Vec[UInt]]](
    size,
    Pixel(Vec(size, UInt(inputWidth.W))),
    biases
  ))

  io.inData <> activation.io.inData
  io.outData <> activation.io.outData

  /*
  val inValid = RegInit(false.B)
  val inBits = Reg(Pixel(Vec(size, UInt(inputWidth.W))))

  val outValid = RegInit(false.B)
  val outBits = Reg(Pixel(Vec(size, Bool())))

  val activation = Module(new BinaryActivation(size, inputWidth, biases))

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
  */
}