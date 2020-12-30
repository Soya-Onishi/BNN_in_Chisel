package bnn

import chisel3._
import chisel3.util._
import chisel3.experimental._

abstract class BinaryConv2D extends BinaryLayer

@chiselName
class BinaryConv2DBinary(
  kernelSize: (Int, Int),
  weightss: Seq[Seq[Boolean]],
  inputSize: Int,
  inputShape: (Int, Int, Int),
  cyclesForAllWeights: Int,
  stride: Int
) extends BinaryConv2D {
  val (kernelH, kernelW) = kernelSize
  val (inputH, inputW, inputC) = inputShape
  val weightsLength = kernelH * kernelW
  val weightsPerCycle = math.ceil(weightss.length.toFloat / cyclesForAllWeights.toFloat).toInt
  val weightIdxMax = math.ceil(weightss.length.toFloat / weightsPerCycle.toFloat).toInt - 1
  val outputSize = weightsPerCycle
  val bitWidth = weightss.head.length * inputC
  val outUIntWidth = unsignedBitLength(bitWidth)
  val weightsss = weightss.sliding(weightsPerCycle, weightsPerCycle).toVector

  assert(inputC >= inputSize)
  assert(weightss.nonEmpty)
  assert(weightss.head.length == weightsLength)

  val io = IO(new Bundle {
    val inData = Flipped(DecoupledIO(Pixel(Vec(inputSize, Bool()))))
    val outData = DecoupledIO(Pixel(Vec(outputSize, UInt(outUIntWidth.W))))

    val isInit = Output(Bool())
  })

  val (weightIdxCounter, weightIdx) = DeluxeCounter(weightIdxMax)
  val layerBase = Module(new Binary2DLayer(kernelSize, inputSize, inputShape, stride, Bool()))
  val bitCounters = Seq.fill(weightsPerCycle)(BitCounter(bitWidth))

  layerBase.io.inData <> io.inData
  layerBase.io.outData.ready := false.B

  io.outData.bits             := DontCare
  io.outData.valid            := false.B
  io.outData.bits.left        := layerBase.io.isLeft
  io.outData.bits.right       := layerBase.io.isRight
  io.outData.bits.topLeft     := layerBase.io.isTopLeft
  io.outData.bits.bottomRight := layerBase.io.isBottomRight
  io.outData.bits.valid       := false.B
  io.isInit                   := layerBase.io.isInit


  // process body (convolution part)
  //
  // - xor and count result bits between weights and kernel
  // - output count result each cycle
  // - Maybe, process cycles are over one cycle when weightsPerCycle < weightss.length ( ceil(weightss.length / weightsPerCycle) )
  // - For now, bit counter does not support multi cycle process, but this module can do that,
  //   so if implement as like that, process cycle is more longer than weightsPerCycle (i.e. cycles > ceil(weightss.length / weightsPerCycle) )
  when(layerBase.io.outData.valid) {
    val defaultWeight = VecInit(Seq.fill(weightsPerCycle)(VecInit(Seq.fill(weightsLength)(false.B))))
    val weightss = MuxLookup(weightIdx.current, defaultWeight, weightsss.zipWithIndex.map {
      case (weightss, idx) =>
        val padding = Seq.fill(weightsPerCycle - weightss.length)(defaultWeight.head)
        val weightssVecBase = weightss.map(weights => VecInit(weights.map(_.B)))
        val weightssVec = weightssVecBase ++ padding

        idx.U(unsignedBitLength(weightIdxMax).max(1).W) -> VecInit(weightssVec)
    })

    val weighted = weightss.map(weights => (weights zip layerBase.io.outData.bits).flatMap { case (w, pixels) => pixels.bits.map(w ^ _) })
    val counts = (weighted zip bitCounters).map{
      case (v, counter) =>
        counter.io.in := VecInit(v)
        counter.io.count
    }

    io.outData.bits.bits  := VecInit(counts)
    io.outData.bits.valid := true.B
    io.outData.valid      := true.B

    when(io.outData.ready) {
      weightIdxCounter.count()
      when(weightIdx.wrapped) {
        layerBase.io.outData.ready := true.B
      }
    }
  }
}

@chiselName
class BinaryConv2DUInt (
  kernelSize: (Int, Int),
  weightss: Seq[Seq[Boolean]],
  inputSize: Int,
  inputShape: (Int, Int, Int),
  cyclesForAllWeights: Int,
  pixelsPerCycle: Int,
  stride: Int,
  inWidth: Int
) extends BinaryConv2D {
  val (kernelH, kernelW) = kernelSize
  val (inputH, inputW, inputC) = inputShape
  val weightsLength = kernelH * kernelW

  val weightsPerCycle = math.ceil(weightss.head.length.toFloat / cyclesForAllWeights.toFloat).toInt
  val weightCounts = math.ceil(weightss.head.length.toFloat / weightsPerCycle.toFloat).toInt
  val weightIdxMax = weightCounts - 1

  val pixelCounts = math.ceil((kernelH * kernelW).toFloat / pixelsPerCycle.toFloat).toInt
  val pixelIdxMax = pixelCounts - 1

  val outputSize = weightsPerCycle
  val bitWidth = weightss.head.length * inputC
  val outSIntWidth = unsignedBitLength(bitWidth) + inWidth

  val weightssPad = (weightss ++ Seq.fill(weightss.length % weightsPerCycle)(Seq.fill(weightsLength)(false))).map {
    weights => weights ++ Seq.fill(weights.length % pixelsPerCycle)(false)
  }
  val weightssss = weightssPad.map(_.sliding(pixelsPerCycle, pixelsPerCycle).toVector).sliding(weightsPerCycle, weightsPerCycle).toVector.map(_.transpose)

  assert(inputC >= inputSize)
  assert(weightss.nonEmpty)
  assert(weightss.head.length == weightsLength)

  val io = IO(new Bundle {
    val inData = Flipped(DecoupledIO(Pixel(Vec(inputSize, UInt(inWidth.W)))))
    val outData = DecoupledIO(Pixel(Vec(outputSize, SInt(outSIntWidth.W))))

    val isInit = Output(Bool())
  })

  val (weightIdxCounter, weightIdx) = DeluxeCounter(weightIdxMax)
  val (pixelIdxCounter, pixelIdx) = DeluxeCounter(pixelIdxMax)
  val (channelIdxCounter, channelIdx) = DeluxeCounter(inputC - 1)
  val layerBase = Module(new Binary2DLayer(kernelSize, inputSize, inputShape, stride, UInt(inWidth.W)))
  val bitCounters = Seq.fill(weightsPerCycle)(BitCounter(bitWidth))
  val convTmpBuffers = RegInit(VecInit(Seq.fill(weightsPerCycle)(0.S(outSIntWidth.W))))

  layerBase.io.inData <> io.inData
  layerBase.io.outData.ready := false.B

  io.outData.bits             := DontCare
  io.outData.valid            := false.B
  io.outData.bits.left        := layerBase.io.isLeft
  io.outData.bits.right       := layerBase.io.isRight
  io.outData.bits.topLeft     := layerBase.io.isTopLeft
  io.outData.bits.bottomRight := layerBase.io.isBottomRight
  io.outData.bits.valid       := false.B
  io.isInit                   := layerBase.io.isInit

  when(layerBase.io.outData.valid) {
    val defaultWeightss = VecInit(Seq.fill(weightCounts)(VecInit(Seq.fill(pixelCounts)(false.B))))
    val weightss = MuxLookup(weightIdx.current, defaultWeightss, weightssss.zipWithIndex.map {
      case (weightsss, idx) =>
        val i = idx.U(unsignedBitLength(weightIdxMax).max(1).W)
        i -> MuxLookup(pixelIdx.current, defaultWeightss, weightsss.zipWithIndex.map {
          case (weightss, idx) =>
            val bitss = VecInit(weightss.map(weights => VecInit(weights.map(_.B))))
            idx.U(unsignedBitLength(pixelIdxMax).max(1).W) -> bitss
        })
    })

    val pixelss = layerBase.io.outData.bits.sliding(pixelsPerCycle, pixelsPerCycle).toVector
    val pixels = MuxLookup(pixelIdx.current, VecInit(Seq.fill(weightsPerCycle)(0.U(inWidth.W))), pixelss.zipWithIndex.map {
      case (pixels, idx) =>
        val bits = pixels.map{ p => p.bits(channelIdx.current) }
        val padding = Seq.fill(weightsPerCycle - pixels.length)(0.U(inWidth.W))
        val appliedPixelBits = bits ++ padding

        idx.U(unsignedBitLength(pixelIdxMax).max(1).W) -> VecInit(appliedPixelBits)
    })

    val weighteds = (convTmpBuffers zip weightss).map {
      case (buf, weights) =>
        val sintPixels = pixels.map(_.pad(outSIntWidth)).map(_.asSInt())
        (weights zip sintPixels).map{ case (w, p) => Mux(w, p, -p) }.foldLeft(buf)(_ + _)
    }

    io.outData.valid      := pixelIdx.wrapped & channelIdx.wrapped
    io.outData.bits.valid := true.B
    io.outData.bits.bits  := VecInit(weighteds)

    convTmpBuffers := MuxCase(VecInit(weighteds), Seq(
      (channelIdx.wrapped & pixelIdx.wrapped & io.outData.ready) -> VecInit(Seq.fill(outputSize)(0.S(outSIntWidth.W))),
      (channelIdx.wrapped & pixelIdx.wrapped)                    -> convTmpBuffers,
    ))

    when(!(channelIdx.wrapped & pixelIdx.wrapped & weightIdx.wrapped & !io.outData.ready)) {
      channelIdxCounter.count()
      when(channelIdx.wrapped) {
        pixelIdxCounter.count()
        when(pixelIdx.wrapped) {
          weightIdxCounter.count()
          when(weightIdx.wrapped) {
            layerBase.io.outData.ready := true.B
          }
        }
      }
    }
  }
}