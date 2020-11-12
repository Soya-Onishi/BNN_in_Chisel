package bnn

import scala.util.Random
import chisel3.iotesters
import chisel3.iotesters.{ChiselFlatSpec, PeekPokeTester}

class BinaryConv2DTester(
  conv: BinaryConv2D,
  weights: Seq[Seq[Boolean]],
  bias: Seq[Int],
  inputShape: (Int, Int, Int),
  stride: Int,
  weightCycles: Int,
  rnd: Random
) extends PeekPokeTester(conv) {
  val (inputH, inputW, inputC) = inputShape
  val images = Seq.fill(inputH)(Seq.fill(inputW)(Seq.fill(inputC)(rnd.nextBoolean())))
  val (kernelH, kernelW) = (conv.kernelH, conv.kernelW)
  val memDepth = math.ceil((inputW - kernelW).toFloat / stride.toFloat).toInt

  poke(conv.io.outData.ready, true)

  expect(conv.io.isInit, true)
  step(memDepth)
  expect(conv.io.isInit, false)

  for {
    y <- 0 until conv.kernelH
    x <- 0 until inputW
  } {
    expect(conv.io.inData.ready, true)
    poke(conv.io.inData.valid, true)
    poke(conv.io.inData.bits.valid, true)
    (conv.io.inData.bits.bits zip images(y)(x)).foreach{ case (in, b) => poke(in, b) }
    step(1)
  }



  for(_ <- 0 until weightCycles) {
    poke(conv.io.inData.valid, false)
    expect(conv.io.inData.ready, true)
    expect(conv.io.outData.valid, false)
    step(1)
  }

  expect(conv.io.outData.valid, true)
}

class BNNTestSpec extends ChiselFlatSpec {
  "binary convolutional layer" should "works correctly" in {
    val rnd = new Random(0)

    val kernelH = 3
    val kernelW = 3
    val kernelSize = (kernelH, kernelW)
    val filterNum = 9
    val weightsCycle = 3
    val stride = 1

    val weights = Seq.fill(filterNum)(Seq.fill(kernelH * kernelW)(rnd.nextBoolean()))
    val bias = Seq.fill(filterNum)(rnd.nextInt(kernelH * kernelW * filterNum))
    val inputShape = (32, 32, 3)

    val backend = "treadle"
    val args = Array("--backend-name", backend, "--generate-vcd-output", "on")

    lazy val conv = new BinaryConv2D(kernelSize, weights, bias, inputShape, weightsCycle, stride)
    iotesters.Driver.execute(args, () => conv) {
      c => new BinaryConv2DTester(c, weights, bias, inputShape, stride, weightsCycle, rnd)
    } should be (true)
  }
}
