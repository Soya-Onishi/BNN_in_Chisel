package bnn

import chisel3._
import chisel3.util._
import chisel3.experimental._

@chiselName
class BinaryActivation(
  val size: Int,
  val inputWidth: Int,
  val biases: Seq[Int]
) extends Module {
  val io = IO(new Bundle {
    val inData = Flipped(DecoupledIO(Vec(size, UInt(inputWidth.W))))
    val outData = DecoupledIO(Vec(size, Bool()))
  })
  val biasIdxMax = math.ceil(biases.length.toFloat / size.toFloat).toInt - 1
  val biasWidth = unsignedBitLength(biases.max)

  val (biasCounter, biasIdx) = DeluxeCounter(biasIdxMax)
  val biasess = WireInit(VecInit(biases.map(_.U(biasWidth.W)).sliding(size, size).map(v => VecInit(v)).toSeq))
  val activateds = (io.inData.bits zip biasess(biasIdx.current)).map{ case (v, bias) => v > bias }

  when(io.inData.valid && io.outData.ready) {
    biasCounter.count()
  }

  io.outData.bits := VecInit(activateds)
  io.outData.valid := io.inData.valid
  io.inData.ready := io.outData.ready
}