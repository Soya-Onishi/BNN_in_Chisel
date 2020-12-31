package bnn

import scala.util.Random
import chisel3._
import chisel3.iotesters
import chisel3.iotesters.{ChiselFlatSpec, PeekPokeTester}

class BinaryConv2DTester(
  conv: BinaryConv2DBinary,
  weightss: Seq[Seq[Boolean]],
  inputSize: Int,
  inputShape: (Int, Int, Int),
  stride: Int,
  idleCycle: Int,
  applyCount: Int,
  countsForAllWeights: Int,
  rnd: Random
) extends PeekPokeTester(conv) {
  val (inputH, inputW, inputC) = inputShape
  val inCountMax = math.ceil(inputC.toFloat / inputSize.toFloat).toInt
  val images = Seq.fill(inputH)(Seq.fill(inputW)((Seq.fill(inputC)(rnd.nextBoolean()) ++ Seq.fill(inCountMax * inputSize - inputC)(false)).sliding(inputSize, inputSize).toSeq))
  val (kernelH, kernelW) = (conv.kernelH, conv.kernelW)
  val memDepth = math.ceil((inputW - kernelW).toFloat / stride.toFloat).toInt
  val outputSize = math.ceil(weightss.length.toFloat / countsForAllWeights.toFloat).toInt
  val outCountMax = math.ceil(weightss.length.toFloat / conv.outputSize.toFloat).toInt

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
    var inCount = 0
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
        } yield images(y)(x).flatten.take(inputC)

        val expected = weightss
          .map { weights => (weights zip cropped).map { case (w, p) => p.map(_ ^ w).count(identity) }.sum }
          .sliding(outputSize, outputSize)
          .toVector

        expect(conv.io.outData.bits.left, outX == 0, s"conv for [$imageX, $imageY, $outCount]")
        expect(conv.io.outData.bits.right, outX == outW - 1, s"conv for [$imageX, $imageY, $outCount]")
        expect(conv.io.outData.bits.topLeft, outX == 0 && outY == 0, s"conv for [$imageX, $imageY, $outCount]")
        expect(conv.io.outData.bits.bottomRight, outX == outW - 1 && outY == outH - 1, s"conv for [$imageX, $imageY, $outCount]")
        (conv.io.outData.bits.bits zip expected(outCount)).foreach {
          case (b, e) =>
            expect(b, BigInt(e), s"convolution for [$imageX, $imageY, $outCount]")
            if(peek(b) != BigInt(e)) {
              val croppedStr = cropped.transpose.map(row => "[" + row.map(b => if(b) 1 else 0).mkString(",") + "]").mkString("\n")
              val weightsStr = weightss.map(row => "[" + row.map(b => if(b) 1 else 0).mkString(",") + "]").mkString("\n")
              logger.info(
                s"""image =>
                  |$croppedStr
                  |""".stripMargin
              )
              logger.info(
                s"""weight =>
                   |$weightsStr
                   |""".stripMargin
              )
              step(1)
              finish
              throw new Exception
            }
        }

        val nextCount = outCount + 1
        outCount = nextCount % outCountMax
        if(nextCount == outCountMax) {
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

      poke(conv.io.inData.valid, sendingPixels)
      images(inY)(inX)(inCount).zip(conv.io.inData.bits.bits).foreach { case (c, b) => poke(b, c) }
      poke(conv.io.inData.bits.valid, true)
      poke(conv.io.inData.bits.left, inX == 0)
      poke(conv.io.inData.bits.right, inX == inputW - 1)
      poke(conv.io.inData.bits.topLeft, inX == 0 && inY == 0)
      poke(conv.io.inData.bits.bottomRight, inX == inputW - 1 && inY == inputH - 1)

      if (peek(conv.io.inData.ready) == 1 && sendingPixels) {
        val nextCount = inCount + 1
        val nextX = inX + 1
        val nextY = inY + 1

        inCount = nextCount % inCountMax
        if(nextCount == inCountMax) {
          inX = nextX % inputW
          if (nextX == inputW) {
            inY = nextY % inputH
          }
        }

        sendingPixels = !((nextX == inputW) && (nextY == inputH) && (nextCount == inCountMax))
      }

      step(1)
    }

    if (idleCount == idleCycle) {
      logger.error(s"reach max idleCount: $idleCycle")
      fail
    }
  }
}

class BinaryConv2DUIntTester(
  conv: BinaryConv2DUInt,
  weightss: Seq[Seq[Boolean]],
  inputSize: Int,
  inputShape: (Int, Int, Int),
  stride: Int,
  idleCycle: Int,
  applyCount: Int,
  rnd: Random
) extends PeekPokeTester(conv) {
  val (inputH, inputW, inputC) = inputShape
  val channelSize = math.ceil(inputC.toFloat / inputSize.toFloat).toInt * inputSize
  val inCountMax = math.ceil(inputC.toFloat / inputSize.toFloat).toInt
  val images = Seq.fill(inputH)(Seq.fill(inputW)((Seq.fill(inputC)(rnd.nextInt(256)) ++ Seq.fill(channelSize - inputC)(0)).sliding(inputSize, inputSize).toSeq))
  val (kernelH, kernelW) = (conv.kernelH, conv.kernelW)
  val outW = (inputW - kernelW) / stride + 1
  val outH = (inputH - kernelH) / stride + 1
  val outCountMax = conv.weightCounts
  val wpc = conv.weightsPerCycle
  val weightsss = weightss.sliding(wpc, wpc).toSeq

  var executing = true
  var inX = 0
  var inY = 0
  var inCount = 0
  var outX = 0
  var outY = 0
  var outCount = 0
  var appliedCount = 0
  var idleCount = 0
  var cycles = 0

  poke(conv.io.outData.ready, true)
  while(executing && idleCount < idleCycle) {
    if(inX == 0 && inY == 0 && inCount == 0 && peek(conv.io.inData.ready) == 1 && appliedCount < applyCount) {
      logger.info(s"begin ${appliedCount}th feeding...")
    }

    poke(conv.io.inData.valid, appliedCount < applyCount)
    poke(conv.io.inData.bits.topLeft, inX == 0 && inY == 0)

    poke(conv.io.inData.bits.left, inX == 0)
    poke(conv.io.inData.bits.right, inX == inputW - 1)
    poke(conv.io.inData.bits.bottomRight, inX == inputW - 1 && inY == inputH - 1)
    poke(conv.io.inData.bits.valid, true)
    (images(inY)(inX)(inCount) zip conv.io.inData.bits.bits).foreach{ case (c, in) => poke(in, c) }

    if(peek(conv.io.inData.ready) == 1 & appliedCount < applyCount) {
      val nextCount = inCount + 1
      val nextX = inX + 1
      val nextY = inY + 1

      inCount = nextCount % inCountMax
      if(nextCount == inCountMax) {
        inX = nextX % inputW
        if(nextX == inputW) {
          inY = nextY % inputH
        }
      }

      if(nextX == inputW && nextY == inputH && nextCount == inCountMax) {
        appliedCount += 1
        if(appliedCount == applyCount) {
          logger.info("feeding images done")
        }
      }
    }

    if(peek(conv.io.outData.valid) == 1) {
      val cropped = for {
        y <- outY * stride until outY * stride + kernelH
        x <- outX * stride until outX * stride + kernelW
      } yield images(y)(x).flatten.take(inputC)

      val expects = weightsss(outCount).map {
        weights => (weights zip cropped).map { case (w, cs) => cs.map(c => if(w) c else -c).sum }.sum
      }
      assert(expects.length == conv.io.outData.bits.bits.length)
      expect(conv.io.outData.bits.valid, true)
      expect(conv.io.outData.bits.left, outX == 0)
      expect(conv.io.outData.bits.right, outX == outW - 1)
      expect(conv.io.outData.bits.topLeft, outX == 0 && outY == 0)
      expect(conv.io.outData.bits.bottomRight, outX == outW - 1 && outY == outH - 1)
      (expects zip conv.io.outData.bits.bits).foreach {
        case (e, a) =>
          expect(a, e, s"[$outX, $outY, $outCount] e = $e, a = ${peek(a)}")
          if(peek(a) != e) {
            val weightStr = weightsss(outCount).map(weights => s"[${weights.map(b => if(b) 1 else 0).mkString(",")}]").mkString("\n")
            val imageStr = images(outY)(outY).flatten
            fail
            finish
            executing = false
          }
      }

      val nextCount = outCount + 1
      val nextX = outX + 1
      val nextY = outY + 1

      outCount = nextCount % outCountMax
      if(nextCount == outCountMax) {
        outX = nextX % outW
        if(nextX == outW) {
          outY = nextY % outH
        }
      }

      if(nextX == outW && nextY == outH && nextCount == outCountMax && appliedCount >= applyCount) {
        executing = false
      }
    }

    if((peek(conv.io.inData.ready) == 0 && appliedCount < applyCount) && peek(conv.io.outData.valid) == 0) {
      idleCount += 1
    } else {
      idleCount = 0
    }

    step(1)
  }

  if(idleCount >= idleCycle) {
    logger.error("reach max cycle")
    fail
    finish
  }
}

class BNNTestSpec extends ChiselFlatSpec {
  def binaryConvTest(
    kernelSize: (Int, Int) = (3, 3),
    inputSize: Int = 3,
    inputShape: (Int, Int, Int) = (18, 18, 3),
    filterNum: Int = 9,
    weightsCycle: Int = 3,
    stride: Int = 1,
    applyCount: Int = 1
  ): Unit = {
    val rnd = new Random(0)
    val (kernelH, kernelW) = kernelSize

    val weights = Seq.fill(filterNum)(Seq.fill(kernelH * kernelW)(rnd.nextBoolean()))

    val backend = "treadle"
    val args = Array("--backend-name", backend, "--generate-vcd-output", "on", "--no-dce")

    lazy val conv = new BinaryConv2DBinary(kernelSize, weights, inputSize, inputShape, weightsCycle, stride)
    iotesters.Driver.execute(args, () => conv) {
      c => new BinaryConv2DTester(c, weights, inputSize, inputShape, stride, 200, applyCount, weightsCycle, rnd)
    } should be(true)
  }


  "binary convolutional layer" should "works correctly" in {
    binaryConvTest()
  }

  "binary convolutional layer two stride" should "works correctly" in {
    binaryConvTest(stride = 2, inputShape = (12, 12, 4))
  }

  "binary convolution layer 2 stride and input shape (9, 9, 3) (i.e. indivisible against input height and width)" should "works correctly" in {
    binaryConvTest(stride = 2, inputShape = (9, 9, 3))
  }

  "binary convolutional layer two stride multi times" should "works correctly" in {
    binaryConvTest(stride = 2, applyCount = 2)
  }

  "binary convolution layer 2 input per once against 3 channel" should "works correctly" in {
    binaryConvTest(inputSize = 2)
  }

  "binary convolutional layer for multiple images" should "works correctly" in {
    binaryConvTest(applyCount = 3)
  }

  "binary convolution layer too many weights cycles" should "works correctly" in {
    binaryConvTest(weightsCycle = 15)
  }

  "binary convolution layer weights cycles indivisible" should "works correctly" in {
    binaryConvTest(weightsCycle = 4)
    binaryConvTest(weightsCycle = 2)
    binaryConvTest(weightsCycle = 5)
  }

  "uint8 convolution layer" should "works correctly" in {
    val rnd = new Random(0)

    val kernelH = 3
    val kernelW = 3
    val kernelSize = (kernelH, kernelW)
    val inputC = 3
    val inputShape = (18, 18, inputC)
    val filterNum = 9
    val weightsCycle = 3
    val pixelsPerCycle = 3
    val idleCycle = 200
    val stride = 1

    val weightss = Seq.fill(filterNum)(Seq.fill(kernelH * kernelW)(rnd.nextBoolean()))

    val backend = "treadle"
    val args = Array("--backend-name", backend, "--generate-vcd-output", "on")

    lazy val conv = new BinaryConv2DUInt(kernelSize, weightss, inputC, inputShape, weightsCycle, pixelsPerCycle, stride, 8)
    iotesters.Driver.execute(args, () => conv) {
      c => new BinaryConv2DUIntTester(c, weightss, inputC, inputShape, stride, idleCycle, 1, rnd)
    } should be (true)
  }

  "uint8 convolution layer multi times" should "works correctly" in {
    val rnd = new Random(0)

    val kernelH = 3
    val kernelW = 3
    val kernelSize = (kernelH, kernelW)
    val inputC = 3
    val inputShape = (18, 18, inputC)
    val filterNum = 9
    val weightsCycle = 3
    val pixelsPerCycle = 3
    val idleCycle = 200
    val stride = 1

    val weightss = Seq.fill(filterNum)(Seq.fill(kernelH * kernelW)(rnd.nextBoolean()))

    val backend = "treadle"
    val args = Array("--backend-name", backend, "--generate-vcd-output", "on")

    lazy val conv = new BinaryConv2DUInt(kernelSize, weightss, inputC, inputShape, weightsCycle, pixelsPerCycle, stride, 8)
    iotesters.Driver.execute(args, () => conv) {
      c => new BinaryConv2DUIntTester(c, weightss, inputC, inputShape, stride, idleCycle, 2, rnd)
    } should be (true)
  }
}
