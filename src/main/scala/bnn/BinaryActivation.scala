package bnn

import chisel3._
import chisel3.util._
import chisel3.experimental._

trait ActivationUtil[T] {
  type RetType <: Data
  type BiasType <: Data

  def genRetType(size: Int): RetType
  def activation(data: T, bias: Vec[Valid[BiasType]]): RetType
  def toBiases(biases: Seq[(Boolean, Int)]): Seq[Valid[BiasType]]
}

@chiselName
class BinaryActivation[T <: Data : ActivationUtil](
  val size: Int,
  val inType: T,
  val biases: Seq[Int]
) extends Module {
  val util = implicitly[ActivationUtil[T]]

  val io = IO(new Bundle {
    val inData = Flipped(DecoupledIO(inType))
    val outData = DecoupledIO(util.genRetType(size))
  })

  val inDefault = Wire(Valid(inType))
  inDefault.valid := false.B
  inDefault.bits := DontCare
  val outDefault = Wire(Valid(util.genRetType(size)))
  outDefault.valid := false.B
  outDefault.bits := DontCare
  val in = RegInit(inDefault)
  val out = RegInit(outDefault)

  val biasIdxMax = math.ceil(biases.length.toFloat / size.toFloat).toInt - 1

  val (biasCounter, biasIdx) = DeluxeCounter(biasIdxMax)
  val validBiases = biases.map(b => (true, b)) ++ Seq.fill(biases.length % size)((false, 0))
  val biasesBase = util.toBiases(validBiases)
  val biasess = WireInit(VecInit(biasesBase.sliding(size, size).map(v => VecInit(v)).toSeq))
  val activateds = util.activation(in.bits, biasess(biasIdx.current))

  when(in.valid && io.outData.ready) {
    biasCounter.count()
  }

  when(io.outData.ready) {
    in.valid := io.inData.valid
    in.bits  := io.inData.bits
    out.valid := in.valid
    out.bits := activateds
  }

  io.outData.bits := out.bits
  io.outData.valid := out.valid
  io.inData.ready := io.outData.ready
}