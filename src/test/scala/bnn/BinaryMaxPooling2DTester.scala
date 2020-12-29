package bnn

import chisel3.iotesters
import chisel3.iotesters.{ChiselFlatSpec, PeekPokeTester}

class BinaryMaxPooling2DTester(
  mp: BinaryMaxPooling2D,
  kernelSize: (Int, Int),
  inputSize: (Int, Int, Int),
  stride: Int,
  applyCount: Int,
  idleCycle: Int
) extends PeekPokeTester(mp) {
  val (kernelH, kernelW) = kernelSize
  val (inputH, inputW, inputC) = inputSize
  val outputH = (inputH - kernelH) / stride + 1
  val outputW = (inputW - kernelW) / stride + 1
  val images = Seq.fill(inputH)(Seq.fill(inputW)(Seq.fill(inputC)(rnd.nextBoolean())))

  poke(mp.io.outData.ready, 1)
  poke(mp.io.inData.valid, 0)

  while(peek(mp.io.isInit) == 1) {
    step(1)
  }

  for(_ <- 0 until applyCount) {
    var inX = 0
    var inY = 0
    var outX = 0
    var outY = 0
    var sendingInput = true
    var executingMP = true
    var idleCount = 0

    while((sendingInput | executingMP) && idleCount < idleCycle) {
      val isIdle = peek(mp.io.outData.valid) == 0 && (peek(mp.io.inData.ready) == 0 || !sendingInput)
      if(isIdle) {
        idleCount += 1
      } else {
        idleCount = 0
      }

      if(peek(mp.io.outData.valid) == 1) {
        val imageX = outX * stride
        val imageY = outY * stride
        // logger.info(s"mp for [$imageX, $imageY]")

        val cropped = for {
          y <- imageY until imageY + kernelH
          x <- imageX until imageX + kernelW
        } yield images(y)(x)

        val expected = cropped.reduceLeft[Seq[Boolean]]{ case (acc, pixel) => (acc zip pixel).map{ case (a, p) => a | p } }
        expect(mp.io.outData.bits.left, outX == 0, s"mp for [$imageX, $imageY]")
        expect(mp.io.outData.bits.right, outX == outputW - 1, s"mp for [$imageX, $imageY]")
        expect(mp.io.outData.bits.topLeft, outX == 0 && outY == 0, s"mp for [$imageX, $imageY]")
        expect(mp.io.outData.bits.bottomRight, outX == outputW - 1 && outY == outputH - 1, s"mp for [$imageX, $imageY]")
        (mp.io.outData.bits.bits zip expected).foreach {
          case (b, e) => expect(b, BigInt(e.toInt), s"mp for [$imageX, $imageY]")
        }

        val nextX = outX + 1
        val nextY = outY + 1
        val mpDone = nextX == outputW && nextY == outputH

        outX = nextX % outputW
        if(nextX == outputW) {
          outY = nextY % outputH
        }

        if(mpDone) {
          logger.info(s"next coordinate: [$nextX, $nextY, $outputW, $outputH, ${nextX == outputW && nextY == outputH}]")
          executingMP = false
        }
      }

      poke(mp.io.inData.valid, sendingInput)
      images(inY)(inX).zip(mp.io.inData.bits.bits).foreach{ case (c, b) => poke(b, c) }
      poke(mp.io.inData.bits.valid, true)
      poke(mp.io.inData.bits.left, inX == 0)
      poke(mp.io.inData.bits.right, inX == inputW - 1)
      poke(mp.io.inData.bits.topLeft, inX == 0 && inY == 0)
      poke(mp.io.inData.bits.bottomRight, inX == inputW - 1 && inY == inputH - 1)

      if(peek(mp.io.inData.ready) == 1 && sendingInput) {
        val nextX = inX + 1
        val nextY = inY + 1

        inX = nextX % inputW
        if(nextX == inputW) {
          inY = nextY % inputH
        }

        if(nextX == inputW && nextY == inputH) {
          sendingInput = false
        }
      }

      step(1)
    }

    if (idleCount == idleCycle) {
      logger.error(s"reach max idleCount: $idleCycle")
      logger.error(s"hang at input: [$inX, $inY], output: [$outX, $outY]")
      logger.error(s"inputSize: [$inputH, $inputW], outputSize: [$outputH, $outputW]")
      fail
    }

    step(100)
  }
}

class BinaryMP2DSpec extends ChiselFlatSpec {
  "binary max pooling for one cycle" should "works correctly" in {
    val kernelSize = (3, 3)
    val inputC     = 32
    val inputShape = (23, 23, inputC)
    val stride = 1
    val applyCount = 1
    val idleCycle = 200

    val backend = "treadle"
    val args = Array("--backend-name", backend, "--generate-vcd-output", "on")

    lazy val mp = new BinaryMaxPooling2D(kernelSize, inputC, inputShape, stride)
    iotesters.Driver.execute(args, () => mp) {
      c => new BinaryMaxPooling2DTester(c, kernelSize, inputShape, stride, applyCount, idleCycle)
    } should be (true)
  }

  "binary max pooling twice" should "works correctly" in {
    val kernelSize = (3, 3)
    val inputC = 32
    val inputShape = (23, 23, inputC)
    val stride = 1
    val applyCount = 2
    val idleCycle = 200

    val backend = "treadle"
    val args = Array("--backend-name", backend, "--generate-vcd-output", "on")

    lazy val mp = new BinaryMaxPooling2D(kernelSize, inputC, inputShape, stride)
    iotesters.Driver.execute(args, () => mp) {
      c => new BinaryMaxPooling2DTester(c, kernelSize, inputShape, stride, applyCount, idleCycle)
    } should be (true)
  }
}
