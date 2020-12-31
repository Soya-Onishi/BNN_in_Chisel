package bnn

import chisel3._
import chisel3.util._

class BinaryActivationDense(
  val size: Int,
  val inputWidth: Int,
  val biases: Seq[Int]
) extends BinaryLayer {
  val io = IO(new Bundle {
    val inData = Flipped(DecoupledIO(Vec(size, UInt(inputWidth.W))))
    val outData = DecoupledIO(Vec(size, Bool()))
  })

  val activation = Module(new BinaryActivation[Vec[UInt]](
    size,
    Vec(size, UInt(inputWidth.W)),
    biases
  ))

  io.inData <> activation.io.inData
  io.outData <> activation.io.outData

  /*
  val inValid = RegInit(false.B)
  val inBits = Reg(Vec(size, UInt(inputWidth.W)))

  val outValid = RegInit(false.B)
  val outBits = Reg(Vec(size, Bool()))

  val activation = Module(new BinaryActivation(size, inputWidth, biases))

  io.inData.ready := io.outData.ready
  when(io.outData.ready) {
    inValid := io.inData.valid
    inBits := io.inData.bits

    outValid := activation.io.outData.valid
    outBits := activation.io.outData.bits
  }

  activation.io.inData.valid := inValid
  activation.io.inData.bits := inBits

  activation.io.outData.ready := io.outData.ready
  io.outData.bits  := outBits
  io.outData.valid := outValid
   */
}