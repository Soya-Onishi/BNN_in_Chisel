package bnn

import chisel3._
import chisel3.util._
import chisel3.iotesters
import chisel3.iotesters.{PeekPokeTester, ChiselFlatSpec}

import scala.util.Random

class Conv2DAndActivation(
  _conv: => BinaryConv2DBool,
  _activation: => BinaryActivationConv2D,
  inputSize: Int,
  outputSize: Int
) extends Module {
  val io = IO(new Bundle {
    val inData = Flipped(DecoupledIO(Pixel(Vec(inputSize, Bool()))))
    val outData = DecoupledIO(Pixel(Vec(outputSize, Bool())))
  })

  val conv = Module(_conv)
  val activation = Module(_activation)

  conv.io.inData <> io.inData
  activation.io.inData <> conv.io.outData
  io.outData <> activation.io.outData
}


class BinaryConv2DActivationTester(
  module: Conv2DAndActivation,
  weightss: Seq[Seq[Boolean]],
  biases: Seq[Int],
  kernelSize: (Int, Int),
  inputShape: (Int, Int, Int),
  stride: Int,
  idleCycle: Int,
  activationSize: Int,
  outIdxMax: Int,
  applyCount: Int,
  rnd: Random
) extends PeekPokeTester(module) {
  val (inputH, inputW, inputC) = inputShape
  val images = Seq.fill(inputH)(Seq.fill(inputW)(Seq.fill(inputC)(rnd.nextBoolean())))
  val (kernelH, kernelW) = kernelSize
  val memDepth = math.ceil((inputW - kernelW).toFloat / stride.toFloat).toInt

  poke(module.io.outData.ready, true)
  poke(module.io.inData.valid, false)

  for (number <- 0 until applyCount) {
    logger.info(s"${number}th cycle")
    var inX = 0
    var inY = 0
    var outX = 0
    var outY = 0
    var outIdx = 0
    var sendingPixels = true
    var executingConv = true
    var idleCount = 0
    val outW = (inputW - kernelW) / stride + 1
    val outH = (inputH - kernelH) / stride + 1

    while (executingConv && idleCount < idleCycle) {
      val isIdle = peek(module.io.outData.valid) == 0 && (peek(module.io.inData.ready) == 0 || !sendingPixels)
      if (isIdle) {
        idleCount += 1
      } else {
        idleCount = 0
      }

      if (peek(module.io.outData.valid) == 1) {
        val imageX = outX * stride
        val imageY = outY * stride

        val cropped = for {
          y <- imageY until imageY + kernelH
          x <- imageX until imageX + kernelW
        } yield images(y)(x)

        def convolution(mat: Seq[Seq[Boolean]]): Int = {
          mat.flatten.foldLeft(0) {
            case (acc, row) => acc + row.toInt
          }
        }

        val applied = weightss.map { weights => (weights zip cropped).map { case (w, p) => p.map(_ ^ w) } }
        val expected = applied.map(convolution).zip(biases)
          .map { case (value, bias) => value > bias }
          .sliding(activationSize, activationSize)
          .toVector(outIdx)

        expect(module.io.outData.bits.left, outX == 0, s"conv for [$imageX, $imageY, $outIdx]")
        expect(module.io.outData.bits.right, outX == outW - 1, s"conv for [$imageX, $imageY, $outIdx]")
        expect(module.io.outData.bits.topLeft, outX == 0 && outY == 0, s"conv for [$imageX, $imageY, $outIdx]")
        expect(module.io.outData.bits.bottomRight, outX == outW - 1 && outY == outH - 1, s"conv for [$imageX, $imageY, $outIdx]")
        (module.io.outData.bits.bits zip expected).foreach {
          case (b, e) => expect(b, BigInt(e.toInt), s"conv for [$imageX, $imageY, $outIdx]")
        }

        outIdx = outIdx + 1
        if(outIdx == outIdxMax + 1) {
          outIdx = 0
          val nextX = outX + 1
          val nextY = outY + 1

          outX = nextX % outW
          if (nextX == outW) {
            outY = nextY % outH
          }

          if (nextX == outW && nextY == outH) {
            executingConv = false
          }
        }
      }

      poke(module.io.inData.valid, sendingPixels)
      images(inY)(inX).zip(module.io.inData.bits.bits).foreach { case (c, b) => poke(b, c) }
      poke(module.io.inData.bits.valid, true)
      poke(module.io.inData.bits.left, inX == 0)
      poke(module.io.inData.bits.right, inX == inputW - 1)
      poke(module.io.inData.bits.topLeft, inX == 0 && inY == 0)
      poke(module.io.inData.bits.bottomRight, inX == inputW - 1 && inY == inputH - 1)

      if (peek(module.io.inData.ready) == 1 && sendingPixels) {
        val nextX = inX + 1
        val nextY = inY + 1

        inX = nextX % inputW
        if (nextX == inputW) {
          inY = nextY % inputH
        }

        sendingPixels = !((nextX == inputW) && (nextY == inputH))
      }

      step(1)
    }

    if (idleCount == idleCycle) {
      logger.error(s"reach max idleCount: $idleCycle")
      fail
    }
  }
}

class BinaryConv2DActivationFlatSpec extends ChiselFlatSpec {
  "combination of conv2D and activation layers" should "works correctly" in {
    val rnd = new Random(0)

    val kernelH = 3
    val kernelW = 3
    val kernelSize = (kernelH, kernelW)
    val inputC = 3
    val inputShape = (18, 18, inputC)
    val filterNum = 9
    val countsForAllWeights = 3
    val idleCycle = 1000
    val stride = 1
    val activationSize = math.ceil(filterNum.toFloat / countsForAllWeights.toFloat).toInt
    val outIdxMax = math.ceil(filterNum.toFloat / activationSize.toFloat).toInt - 1

    val weightss = Seq.fill(filterNum)(Seq.fill(kernelH * kernelW)(rnd.nextBoolean()))
    val biases = Seq.fill(filterNum)(rnd.nextInt(kernelH * kernelW * inputC))

    val backend = "treadle"
    val args = Array("--backend-name", backend, "--generate-vcd-output", "on", "--no-dce")

    lazy val conv = new BinaryConv2DBool(kernelSize, weightss, inputShape, countsForAllWeights, stride)
    lazy val activation = new BinaryActivationConv2D(activationSize, unsignedBitLength(kernelH * kernelW * inputC), biases)
    lazy val top = new Conv2DAndActivation(conv, activation, inputC, activationSize)
    iotesters.Driver.execute(args, () => top) {
      c => new BinaryConv2DActivationTester(
        c,
        weightss,
        biases,
        kernelSize,
        inputShape,
        stride,
        idleCycle,
        activationSize,
        outIdxMax,
        1,
        rnd
      )
    } should be (true)
  }
}