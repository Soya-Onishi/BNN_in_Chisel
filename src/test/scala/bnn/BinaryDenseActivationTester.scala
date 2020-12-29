package bnn

import chisel3._
import chisel3.util._
import chisel3.iotesters.{PeekPokeTester, ChiselFlatSpec}
import scala.util.Random

class DenseAndActivation(
  _dense: => BinaryDense,
  _activation: => BinaryActivationDense,
  inputSize: Int
) extends Module {
  val dense = Module(_dense)
  val activation = Module(_activation)

  val io = IO(new Bundle {
    val inData = Flipped(DecoupledIO(Vec(inputSize, Bool())))
    val outData = DecoupledIO(Vec(dense.outputPacketSize, Bool()))
  })

  dense.io.inData <> io.inData
  activation.io.inData <> dense.io.outData
  io.outData <> activation.io.outData
}

class BinaryDenseActivationTester(
  module: DenseAndActivation,
  weightss: Seq[Seq[Boolean]],
  biases: Seq[Int],
  idleCount: Int,
  rnd: Random
) extends PeekPokeTester(module) {
  val inputNeuron = module.dense.inputNeuron
  val inputSize = module.dense.inputSize
  val outputSize = module.activation.size
  val inputs = Vector.fill(module.dense.inputNeuron)(rnd.nextBoolean())
  val inputss = inputs
    .map(_.toInt)
    .map(BigInt.apply)
    .sliding(inputSize, inputSize).toVector
  val expects = (weightss zip biases).map{ case (weights, bias) =>
    (weights zip inputs).map{ case (w, i) => w ^ i }.count(identity) > bias
  }
  val es = (weightss zip biases).map{ case (weights, bias) =>
    (weights zip inputs).map{ case (w, i) => w ^ i }.count(identity)
  }
  val expectss = expects.sliding(outputSize, outputSize).toVector

  poke(module.io.outData.ready, true)
  poke(module.io.inData.valid, false)

  var inIdx = 0
  var outIdx = 0
  var executing = true
  var idleCycle = 0
  val inDefault = Vector.fill(inputSize)(BigInt(0))

  while(executing && idleCycle < idleCount) {
    poke(module.io.inData.valid, (inIdx < inputss.length).toInt)
    poke(module.io.inData.bits, inputss.lift(inIdx).getOrElse(inDefault))
    if(inIdx < inputss.length & peek(module.io.inData.ready) == 1) {
      inIdx += 1
      idleCycle = 0
    }

    if(outIdx < expectss.length & peek(module.io.outData.valid) == 1) {
      val actuals = peek(module.io.outData.bits)
      val expects = expectss.lift(outIdx)
        .map(es => es ++ Vector.fill(outputSize - es.length)(false))
        .getOrElse(Vector.fill(outputSize)(false))

      expect(actuals.length == expects.length, s"actual and expect length mismatch [actual: ${actuals.length}, expect: ${expects.length}]")
      (expects zip actuals).zipWithIndex.foreach{ case ((e, a), idx) => expect(e == (a == 1), s"[$outIdx, $idx] e = $e, a = ${a == 1}")}

      outIdx += 1
      idleCycle = 0
      if(outIdx == expectss.length) {
        executing = false
      }
    }

    val inIdle = inIdx < inputss.length & peek(module.io.inData.ready) == 0
    val outIdle = outIdx < expectss.length & peek(module.io.outData.valid) == 0
    if(inIdle && outIdle) {
      idleCycle += 1
    }

    step(1)
  }

  if(idleCycle >= idleCount) {
    logger.error("reached idle cycle to max")
    fail
  }
}

class BinaryDenseAndActivationFlatSpec extends ChiselFlatSpec {
  "combination of dense and activation layers" should "works correctly" in {
    val rnd = new Random(0)
    val inputNeuron = 128
    val inputSize = 8
    val cyclesForAllWeights = 4
    val weightNumber = 12
    val activationSize = math.ceil(weightNumber.toFloat / cyclesForAllWeights.toFloat).toInt
    val weightss = Vector.fill(weightNumber)(Vector.fill(inputNeuron)(rnd.nextBoolean()))
    val biases = Vector.fill(weightNumber)(rnd.nextInt(inputNeuron))

    val backend = "treadle"
    val args = Array("--backend-name", backend, "--generate-vcd-output", "on", "--no-dce")

    lazy val dense = new BinaryDense(inputSize, inputNeuron, cyclesForAllWeights, weightss)
    lazy val activation = new BinaryActivationDense(activationSize, unsignedBitLength(inputNeuron), biases)
    lazy val top = new DenseAndActivation(dense, activation, inputSize)
    iotesters.Driver.execute(args, () => top) {
      c => new BinaryDenseActivationTester(
        c,
        weightss,
        biases,
        idleCount = 200,
        rnd
      )
    } should be (true)
  }
}
