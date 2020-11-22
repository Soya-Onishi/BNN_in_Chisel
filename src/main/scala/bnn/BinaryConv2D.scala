package bnn

import chisel3._
import chisel3.util.{Decoupled, Enum, Valid}
import chisel3.util.{SwitchContext, is, switch}

abstract class BinaryConv2D[InputType <: Data] (
  kernelSize: (Int, Int),
  weights: Seq[Seq[Boolean]],
  inputShape: (Int, Int, Int),
  countsForAllWeights: Int,
  stride: Int,
  inputType: InputType
) extends Binary2DLayer[InputType, UInt](
  kernelSize,
  inputShape,
  math.ceil(weights.length.toFloat / countsForAllWeights.toFloat).toInt,
  stride,
  inputType,
  UInt(requiredLength(kernelSize._1 * kernelSize._2 * inputShape._3).W)
)

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
        globalState := wait_to_next
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
