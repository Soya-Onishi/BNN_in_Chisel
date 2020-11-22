package bnn

import chisel3.iotesters.{PeekPokeTester, ChiselFlatSpec}
import scala.util.Random

class BinaryDenseTester(
  dense: BinaryDense,
  inputSize: Int,
  inputNeuron: Int,
  weightSize: Int,
  weightss: Seq[Seq[Boolean]],
  biases: Seq[Int],
  cycles: Int,
  idleCycle: Int,
  countOfInputs: Int,
  rnd: Random
) extends PeekPokeTester(dense) {
  logger.info("init parameters")

  val inputs = Seq.fill(inputNeuron)(rnd.nextBoolean())
  val inputss = inputs.sliding(inputSize, inputSize).toSeq
  val outputSize = math.ceil(weightss.length / cycles.toFloat).toInt
  val results = (weightss zip biases).map{ case (weights, bias) =>
    val mult = (weights zip inputs).map{ case (w, i) => w ^ i }
    val count = mult.count(identity)
    count > bias
  }
  val resultss = results.sliding(outputSize, outputSize).toSeq

  logger.info("parameters have been initialized")

  poke(dense.io.outData.ready, true)
  poke(dense.io.inData.valid, false)

  logger.info("waiting for finishing init state...")
  while(peek(dense.io.isInit) == 1) {
    step(1)
  }
  logger.info("init state is done")

  for(_ <- 0 until countOfInputs) {
    var inputPos = 0
    var outputPos = 0
    var executingDense = true
    var moreInputs = true
    var idleCount = 0

    logger.info("start sending data into dense layer")

    while(executingDense && idleCount < idleCycle) {
      if(peek(dense.io.outData.valid) == 1) {
        val outputs = resultss(outputPos)
        (dense.io.outData.bits zip outputs).foreach{ case (e, a) => expect(e, a) }

        val nextOutput = outputPos + 1
        outputPos = nextOutput % resultss.length
        executingDense = nextOutput == resultss.length
      }

      poke(dense.io.inData.valid, false)

      if(peek(dense.io.inData.ready) == 1 && moreInputs) {
        val inputs = inputss(inputPos)

        poke(dense.io.inData.valid, true)
        (dense.io.inData.bits zip inputs).foreach{ case (b, in) => poke(b, in) }

        val nextInput = inputPos + 1
        inputPos = nextInput % inputss.length
        moreInputs = nextInput == inputss.length
      }

      val isIdle = peek(dense.io.outData.valid) == 0 | (peek(dense.io.inData.ready) == 0 && moreInputs)
      if(isIdle) {
        idleCount += 1
      } else {
        idleCount = 0
      }

      step(1)
    }
  }
}

class BinaryDenseSpec extends ChiselFlatSpec {
  "binary dense layer" should "works correctly" in {
    val rnd = new Random(0)

    val inputSize = 128
    val inputNeuron = 115200
    val weightSize = 128
    val cycles = 4
    val idleCycle = 200

    val weightss = Seq.fill(weightSize)(Seq.fill(inputNeuron)(rnd.nextBoolean()))
    val biases = Seq.fill(weightSize)(rnd.nextInt(inputNeuron))

    val backend = "treadle"
    val args = Array("--backend-name", backend, "--generate-vcd-output", "on")

    lazy val dense = new BinaryDense(inputSize, inputNeuron, cycles, weightss, biases)
    chisel3.iotesters.Driver.execute(args, () => dense) {
      c => new BinaryDenseTester(c, inputSize, inputNeuron, weightSize, weightss, biases, cycles, idleCycle, 1, rnd)
    }
  }
}
