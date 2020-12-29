package bnn

import chisel3._
import chisel3.util._
import chisel3.experimental._

@chiselName
class BinaryConv2DBinary(
  kernelSize: (Int, Int),
  weightss: Seq[Seq[Boolean]],
  inputSize: Int,
  inputShape: (Int, Int, Int),
  cyclesForAllWeights: Int,
  stride: Int
) extends Module {
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
  assert(weightsss.nonEmpty)
  assert(weightsss.head.length == weightsLength)

  val io = IO(new Bundle {
    val inData = Flipped(DecoupledIO(Pixel(Vec(inputSize, Bool()))))
    val outData = DecoupledIO(Pixel(Vec(outputSize, UInt(outUIntWidth.W))))

    val isInit = Output(Bool())
  })

  val (weightIdxCounter, weightIdx) = DeluxeCounter(weightIdxMax)
  val layerBase = Module(new Binary2DLayer(kernelSize, inputSize, inputShape, stride, Bool()))
  val bitCounters = Seq.fill(weightsPerCycle)(Module(new BitCounter(bitWidth)))

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

        idx.U(unsignedBitLength(weightIdxMax)) -> VecInit(weightssVec)
    })

    val weighted = weightss.map(weights => (weights zip layerBase.io.outData.bits).flatMap { case (w, pixels) => pixels.bits.map(w ^ _) })
    val counts = (weighted zip bitCounters).map{
      case (v, counter) =>
        counter.io.in := v
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

/*
class BinaryConv2DBool(
  kernelSize: (Int, Int),
  weights: Seq[Seq[Boolean]],
  inputShape: (Int, Int, Int),
  countsForAllWeights: Int,
  stride: Int,
) extends BinaryConv2D[Bool](
  kernelSize,
  weights,
  inputShape,
  countsForAllWeights,
  stride,
  Bool()
) {
  val weightsPerApply = math.ceil(weights.length.toFloat / countsForAllWeights.toFloat).toInt
  val countsForApplyingAllWeights = math.ceil(weights.length.toFloat / weightsPerApply.toFloat).toInt
  val weightsss = weights.sliding(weightsPerApply, weightsPerApply).toSeq
  val weightsssVec = VecInit(
    weightsss.map(weightss => VecInit(
      weightss.map(weights => VecInit(
        weights.map(weight => weight.asBool())
      ))
    ))
  )

  val weightIdx = RegInit(0.U(requiredLength(countsForAllWeights).W))

  compile()

  override protected def executionState(): Unit = {
    val bitLength = inputC * kernelH * kernelW
    val countLength = requiredLength(bitLength)
    val bitCounters = Seq.fill(weightsPerApply)(Module(new BitCounter(bitLength)))
    bitCounters.foreach(counter => counter.io.in := VecInit(Seq.fill(counter.width)(false.B)))

    val readyForNextPixel = ((nextInputBufferIdx === stride.U) & io.inData.valid) | isInputBufferFull
    val weightss = weightsssVec(weightIdx)

    val counts = (weightss zip bitCounters).map { case (weights, counter) =>
      val bits = (weights zip window.io.window.bits).flatMap {
        case (weight, pixel) => pixel.bits.map(_ ^ weight)
      }

      counter.io.in := VecInit(bits)
      counter.io.count
    }

    val pixel = Wire(Pixel(Vec(weightsPerApply, UInt(countLength.W))))
    pixel.left        := window.io.isLeft
    pixel.right       := window.io.isRight
    pixel.topLeft     := window.io.isTopLeft
    pixel.bottomRight := window.io.isBottomRight
    pixel.valid       := true.B
    pixel.bits        := VecInit(counts)

    isBottomRight    := window.io.isBottomRight
    io.outData.valid := window.io.window.valid
    io.outData.bits  := pixel

    when(io.outData.ready & window.io.window.valid) {
      val nextIdx = weightIdx + 1.U
      weightIdx := Mux(nextIdx === countsForApplyingAllWeights.U, 0.U, nextIdx)
      when(nextIdx === countsForApplyingAllWeights.U) {
        globalState := wait_next
      }
    }

    when(!window.io.window.valid & readyForNextPixel) {
      val reachBottomRight = nextPixelBits.foldLeft(false.B) { case (acc, pixel) => acc | pixel.bottomRight }
      shiftPolicy := Mux(reachBottomRight, force_shift, shiftPolicy)
      window.io.nextPixel.valid := true.B
      inputBufferIdx := 0.U
    }
  }
}
*/