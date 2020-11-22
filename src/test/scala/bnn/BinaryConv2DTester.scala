package bnn

import scala.util.Random
import chisel3._
import chisel3.iotesters
import chisel3.iotesters.{ChiselFlatSpec, PeekPokeTester}

class BinaryConv2DTester(
  conv: BinaryConv2D[Bool],
  weightss: Seq[Seq[Boolean]],
  inputShape: (Int, Int, Int),
  stride: Int,
  idleCycle: Int,
  applyCount: Int,
  countsForAllWeights: Int,
  rnd: Random
) extends PeekPokeTester(conv) {
  val (inputH, inputW, inputC) = inputShape
  val images = Seq.fill(inputH)(Seq.fill(inputW)(Seq.fill(inputC)(rnd.nextBoolean())))
  val (kernelH, kernelW) = (conv.kernelH, conv.kernelW)
  val memDepth = math.ceil((inputW - kernelW).toFloat / stride.toFloat).toInt
  val outputSize = math.ceil(weightss.length.toFloat / countsForAllWeights.toFloat).toInt

  poke(conv.io.outData.ready, true)
  poke(conv.io.inData.valid, false)

  while(peek(conv.io.isInit) == 1) {
    step(1)
  }

  for (number <- 0 until applyCount) {
    logger.info(s"${number}th cycle")
    var inX = 0
    var inY = 0
    var outX = 0
    var outY = 0
    var outCount = 0
    var sendingPixels = true
    var executingConv = true
    var idleCount = 0
    val outW = (inputW - kernelW) / stride + 1
    val outH = (inputH - kernelH) / stride + 1

    while (executingConv && idleCount < idleCycle) {
      val isIdle = peek(conv.io.outData.valid) == 0 && (peek(conv.io.inData.ready) == 0 || !sendingPixels)
      if (isIdle) {
        idleCount += 1
      } else {
        idleCount = 0
      }

      if (peek(conv.io.outData.valid) == 1) {
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
        val expected = applied.map(convolution).sliding(outputSize, outputSize).toSeq

        expect(conv.io.outData.bits.left, outX == 0, s"conv for [$imageX, $imageY, $outCount]")
        expect(conv.io.outData.bits.right, outX == outW - 1, s"conv for [$imageX, $imageY, $outCount]")
        expect(conv.io.outData.bits.topLeft, outX == 0 && outY == 0, s"conv for [$imageX, $imageY, $outCount]")
        expect(conv.io.outData.bits.bottomRight, outX == outW - 1 && outY == outH - 1, s"conv for [$imageX, $imageY, $outCount]")
        (conv.io.outData.bits.bits zip expected(outCount)).foreach {
          case (b, e) => expect(b, BigInt(e), s"conv for [$imageX, $imageY, $outCount]")
        }

        val nextCount = outCount + 1
        outCount = nextCount % countsForAllWeights

        if(nextCount == countsForAllWeights) {
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

      poke(conv.io.inData.valid, peek(conv.io.inData.ready) == 1 && sendingPixels)
      images(inY)(inX).zip(conv.io.inData.bits.bits).foreach { case (c, b) => poke(b, c) }
      poke(conv.io.inData.bits.valid, true)
      poke(conv.io.inData.bits.left, inX == 0)
      poke(conv.io.inData.bits.right, inX == inputW - 1)
      poke(conv.io.inData.bits.topLeft, inX == 0 && inY == 0)
      poke(conv.io.inData.bits.bottomRight, inX == inputW - 1 && inY == inputH - 1)

      if (peek(conv.io.inData.ready) == 1 && sendingPixels) {
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

class BNNTestSpec extends ChiselFlatSpec {
  "binary convolutional layer" should "works correctly" in {
    val rnd = new Random(0)

    val kernelH = 3
    val kernelW = 3
    val kernelSize = (kernelH, kernelW)
    val inputC = 3
    val inputShape = (18, 18, inputC)
    val filterNum = 9
    val weightsCycle = 3
    val idleCycle = 200
    val stride = 1

    val weights = Seq.fill(filterNum)(Seq.fill(kernelH * kernelW)(rnd.nextBoolean()))

    val backend = "treadle"
    val args = Array("--backend-name", backend, "--generate-vcd-output", "on")

    lazy val conv = new BinaryConv2D(kernelSize, weights, inputShape, weightsCycle, stride, Bool())
    iotesters.Driver.execute(args, () => conv) {
      c => new BinaryConv2DTester(c, weights, inputShape, stride, idleCycle, 1, weightsCycle, rnd)
    } should be(true)
  }

  "binary convolutional layer for multiple images" should "works correctly" in {
    val rnd = new Random(0)

    val kernelH = 3
    val kernelW = 3
    val kernelSize = (kernelH, kernelW)
    val inputC = 3
    val inputShape = (18, 18, inputC)
    val filterNum = 9
    val weightsCycle = 3
    val idleCycle = 200
    val stride = 1

    val weights = Seq.fill(filterNum)(Seq.fill(kernelH * kernelW)(rnd.nextBoolean()))

    val backend = "treadle"
    val args = Array("--backend-name", backend, "--generate-vcd-output", "on")

    lazy val conv = new BinaryConv2D(kernelSize, weights, inputShape, weightsCycle, stride, Bool())
    iotesters.Driver.execute(args, () => conv) {
      c => new BinaryConv2DTester(c, weights, inputShape, stride, idleCycle, 2, weightsCycle, rnd)
    } should be(true)
  }
}
