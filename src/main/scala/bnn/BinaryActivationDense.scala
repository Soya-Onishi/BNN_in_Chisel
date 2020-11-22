package bnn

import chisel3._

class BinaryActivationDense(
  val channels: Int,
  val inputSize: Int,
  val inputWidth: Int,
  val biases: Seq[Int],
) extends BinaryActivation[Vec[UInt], Vec[Bool]] (
  channels,
  inputSize,
  inputWidth,
  biases,
  Vec(inputSize, UInt(inputWidth.W)),
  Vec(channels, Bool())
) {
  io.inData.ready := globalState =/= sending
  io.outData.valid := false.B
  io.outData.bits := VecInit(outputBuffer.flatten.take(channels))

  elaborate()

  override protected def renewBuffer(): Unit = {
    when(io.inData.valid & globalState =/= sending) {
      inputBuffer.bits := io.inData.bits
    }
  }
}
