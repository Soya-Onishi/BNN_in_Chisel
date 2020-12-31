package bnn

import chisel3._
import chisel3.util._
import chisel3.iotesters
import chisel3.iotesters.{PeekPokeTester, ChiselFlatSpec}

import scala.util.Random

class Conv2DAndActivationBool(
  _conv: => BinaryConv2DBinary,
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

class Conv2DAndActivationUInt (
  _conv: => BinaryConv2DUInt,
  _activation: => BinaryActivation[Pixel[Vec[SInt]]],
  inputSize: Int,
  outputSize: Int
) extends Module {
  val inputWidth = 8

  val io = IO(new Bundle {
    val inData = Flipped(DecoupledIO(Pixel(Vec(inputSize, UInt(inputWidth.W)))))
    val outData = Decoupled(Pixel(Vec(outputSize, Bool())))
  })

  val conv = Module(_conv)
  val activation = Module(_activation)

  conv.io.inData <> io.inData
  activation.io.inData <> conv.io.outData
  io.outData <> activation.io.outData
}


class BinaryConv2DActivationTester(
  module: Conv2DAndActivationBool,
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

        val expectedBase = weightss
          .map { weights => (weights zip cropped).map { case (w, p) => p.map(_ ^ w).count(identity) }.sum }
          .zip(biases)
          .map{ case (s, b) => s > b }

        val expected = (expectedBase ++ Seq.fill(expectedBase.length % activationSize)(false))
          .sliding(activationSize, activationSize)
          .toVector(outIdx)

        expect(module.io.outData.bits.left, outX == 0, s"conv for [$imageX, $imageY, $outIdx]")
        expect(module.io.outData.bits.right, outX == outW - 1, s"conv for [$imageX, $imageY, $outIdx]")
        expect(module.io.outData.bits.topLeft, outX == 0 && outY == 0, s"conv for [$imageX, $imageY, $outIdx]")
        expect(module.io.outData.bits.bottomRight, outX == outW - 1 && outY == outH - 1, s"conv for [$imageX, $imageY, $outIdx]")
        (module.io.outData.bits.bits zip expected).foreach {
          case (b, e) => expect(b, e, s"conv for [$imageX, $imageY, $outIdx]")
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

class BinaryConv2DActivationUIntTester(
  top: Conv2DAndActivationUInt,
  weightss: Seq[Seq[Boolean]],
  biases: Seq[Int],
  kernelSize: (Int, Int),
  inputSize: Int,
  inputShape: (Int, Int, Int),
  stride: Int,
  idleCycle: Int,
  outputSize: Int,
  applyCount: Int,
  rnd: Random
) extends PeekPokeTester(top) {
  val (inputH, inputW, inputC) = inputShape
  val image = Seq.fill(inputH)(Seq.fill(inputW)(Seq.fill(inputC)(rnd.nextInt(256))))
  val imgPadSize = if(inputC % inputSize == 0) 0 else inputC - inputC % inputSize
  val images = image.map(_.map(cs => (cs ++ Seq.fill(imgPadSize)(0)).sliding(inputSize, inputSize).toSeq))
  val (kernelH, kernelW) = kernelSize
  val inMax = math.ceil(inputC.toFloat / inputSize.toFloat).toInt
  val outMax = math.ceil(weightss.length.toFloat / outputSize.toFloat).toInt
  val outW = (inputW - kernelW) / stride + 1
  val outH = (inputH - kernelH) / stride + 1

  var inX = 0
  var inY = 0
  var inIdx = 0
  var outX = 0
  var outY = 0
  var outIdx = 0
  var executing = true
  var idleCount = 0
  var applyNumber = 0
  var cycles = 0

  poke(top.io.outData.ready, true)
  while(executing && idleCount < idleCycle) {
    if(inIdx == 0 && inX == 0 && inY == 0 && applyNumber < applyCount && peek(top.io.inData.ready) == 1) {
      logger.info(s"sending ${applyNumber + 1}th inputs...")
    }

    poke(top.io.inData.valid, applyNumber < applyCount)
    poke(top.io.inData.bits.valid, true)
    poke(top.io.inData.bits.left, inX == 0)
    poke(top.io.inData.bits.right, inX == inputW - 1)
    poke(top.io.inData.bits.topLeft, inX == 0 && inY == 0)
    poke(top.io.inData.bits.bottomRight, inX == inputW - 1 && inY == inputH - 1)
    (images(inY)(inX)(inIdx) zip top.io.inData.bits.bits).foreach {
      case (in, port) => poke(port, in)
    }
    if(peek(top.io.inData.ready) == 1 && applyNumber < applyCount) {
      val nextIdx = inIdx + 1
      val nextX = inX + 1
      val nextY = inY + 1

      inIdx = nextIdx % inMax
      if(nextIdx == inMax) {
        inX = nextX % inputW
        if(nextX == inputW) {
          inY = nextY % inputH
        }
      }

      if(nextIdx == inMax && nextX == inputW && nextY == inputH) {
        applyNumber += 1
      }
    }

    if(peek(top.io.outData.valid) == 1) {
      val imgX = outX * stride
      val imgY = outY * stride
      val cropped = for {
        y <- imgY until imgY + kernelH
        x <- imgX until imgX + kernelW
      } yield image(y)(x)

      val expects = (weightss zip biases).map { case (weights, bias) =>
        val conv = (cropped zip weights).map { case (pixel, w) => pixel.map(c => if (w) c else -c).sum }.sum
        conv > bias
      }
      val padSize = if(weightss.length % outputSize == 0) 0 else outputSize - weightss.length % outputSize
      val pad = Seq.fill(padSize)(false)
      val expectss = (expects ++ pad).sliding(outputSize, outputSize).toSeq

      val actual = top.io.outData.bits
      expect(actual.valid, true)
      expect(actual.left, outX == 0)
      expect(actual.right, outX == outW - 1)
      expect(actual.topLeft, outX == 0 && outY == 0)
      expect(actual.bottomRight, outX == outW - 1 && outY == outH - 1)
      (actual.bits zip expectss(outIdx)).foreach {
        case (a, e) => expect(a, e, s"conv for [$outX, $outY, $outIdx]")
      }

      val nextIdx = outIdx + 1
      val nextX = outX + 1
      val nextY = outY + 1
      outIdx = nextIdx % outMax
      if(nextIdx == outMax) {
        outX = nextX % outW
        if(nextX == outW) {
          outY = nextY % outH
        }
      }

      if(nextIdx == outMax && nextX == outW && nextY == outH && applyNumber >= applyCount) {
        executing = false
      }
    }

    val inIdle = peek(top.io.inData.ready) == 0 && applyNumber < applyCount
    val outIdle = peek(top.io.outData.valid) == 0
    if(inIdle && outIdle) {
      idleCount += 1
    } else {
      idleCount = 0
    }

    cycles += 1
    if(cycles >= 10000) {
      executing = false
    }
    step(1)
  }

  if(idleCount >= idleCycle) {
    logger.error("reach max idle cycle")
    fail
    finish
  }

  if(cycles >= 10000) {
    logger.error("reach max cycle")
    fail
    finish
  }
}

class BinaryConv2DActivationFlatSpec extends ChiselFlatSpec {
  def runConvActivation(
    kernelSize: (Int, Int) = (3, 3),
    inputShape: (Int, Int, Int) = (18, 18, 3),
    filterNum: Int = 9,
    countsForAllWeights: Int = 3,
    stride: Int = 1,
    idleCycle: Int = 200,
    applyCount: Int = 1
  ): Unit = {
    val rnd = new Random(0)

    val (kernelH, kernelW) = kernelSize
    val (inputH, inputW, inputC) = inputShape
    val activationSize = math.ceil(filterNum.toFloat / countsForAllWeights.toFloat).toInt
    val outIdxMax = math.ceil(filterNum.toFloat / activationSize.toFloat).toInt - 1

    val weightss = Seq.fill(filterNum)(Seq.fill(kernelH * kernelW)(rnd.nextBoolean()))
    val biases = Seq.fill(filterNum)(rnd.nextInt(kernelH * kernelW * inputC))

    val backend = "treadle"
    val args = Array("--backend-name", backend, "--generate-vcd-output", "on", "--no-dce")

    lazy val conv = new BinaryConv2DBinary(kernelSize, weightss, inputC, inputShape, countsForAllWeights, stride)
    lazy val activation = new BinaryActivationConv2D(activationSize, unsignedBitLength(kernelH * kernelW * inputC), biases)
    lazy val top = new Conv2DAndActivationBool(conv, activation, inputC, activationSize)
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
        applyCount,
        rnd
      )
    } should be (true)
  }

  def runConvActivationUInt(
    kernelSize: (Int, Int) = (3, 3),
    inputSize: Int = 3,
    inputShape: (Int, Int, Int) = (18, 18, 3),
    filterNum: Int = 9,
    countsForAllWeights: Int = 3,
    pixelsPerCycle: Int = 3,
    stride: Int = 1,
    idleCycle: Int = 200,
    applyCount: Int = 1
  ): Unit = {
    val rnd = new Random(0)

    val (kernelH, kernelW) = kernelSize
    val (inputH, inputW, inputC) = inputShape
    val activationSize = math.ceil(filterNum.toFloat / countsForAllWeights.toFloat).toInt
    val intSize = 8 + inputH * inputW * inputC
    val inType = Pixel(Vec(activationSize, SInt(intSize.W)))

    val weightss = Seq.fill(filterNum)(Seq.fill(kernelH * kernelW)(rnd.nextBoolean()))
    val biases = Seq.fill(filterNum)(rnd.nextInt(kernelH * kernelW * inputC))

    val backend = "treadle"
    val args = Array("--backend-name", backend, "--generate-vcd-output", "on")

    lazy val conv = new BinaryConv2DUInt(kernelSize, weightss, inputSize, inputShape, countsForAllWeights, pixelsPerCycle, stride, 8)
    lazy val activation = new BinaryActivation[Pixel[Vec[SInt]]](activationSize, inType, biases)
    lazy val top = new Conv2DAndActivationUInt(conv, activation, inputSize, activationSize)

    iotesters.Driver.execute(args, () => top) {
      c => new BinaryConv2DActivationUIntTester(
        c,
        weightss,
        biases,
        kernelSize,
        inputSize,
        inputShape,
        stride,
        idleCycle,
        activationSize,
        applyCount,
        rnd
      )
    } should be (true)
  }

  "combination of conv2D and activation layers" should "works correctly" in {
    runConvActivation()
  }

  "combination of conv2D and activation layers on multi times" should "works correctly" in {
    runConvActivation(applyCount = 2)
  }

  "number of weights % conv per cycle != 0" should "works correctly" in {
    runConvActivation(countsForAllWeights = 4)
    runConvActivation(countsForAllWeights = 2)
  }

  "number of weights % conv per cycle != 0 multi cycle" should "works correctly" in {
    runConvActivation(countsForAllWeights = 4, applyCount = 2)
  }

  "uint convolution and activation" should "works correctly" in {
    runConvActivationUInt()
  }
}