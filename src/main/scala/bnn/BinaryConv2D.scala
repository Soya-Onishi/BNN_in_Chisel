package bnn

import chisel3._
import chisel3.util.{Decoupled, Enum, Valid}
import chisel3.util.{SwitchContext, is, switch}

class BinaryConv2D(
  kernelSize: (Int, Int),
  weights: Seq[Seq[Boolean]],
  bias: Seq[Int],
  inputShape: (Int, Int, Int),
  countsForAllWeights: Int,
  stride: Int
) extends Binary2DLayer(
  kernelSize,
  inputShape,
  weights.length,
  stride
) {
  val weightsPerApply = math.ceil(weights.length.toFloat / countsForAllWeights.toFloat).toInt
  val countsForApplyingAllWeights = math.ceil(weights.length.toFloat / weightsPerApply.toFloat).toInt
  val weightsss = weights.sliding(weightsPerApply, weightsPerApply).toSeq
  val biasess = bias.sliding(weightsPerApply, weightsPerApply).toSeq
  val weightsssVec = VecInit(
    weightsss.map(weightss => VecInit(
      weightss.map(weights => VecInit(
        weights.map(weight => weight.asBool())
      ))
    ))
  )
  val biasessVec = VecInit(
    biasess.map(biases => VecInit(
      biases.map(_.asUInt())
    ))
  )

  require(weights.length == bias.length)

  val weightIdx = RegInit(0.U(requiredLength(countsForAllWeights).W))
  val activatedBuffer = Reg(Vec(countsForApplyingAllWeights, Vec(weightsPerApply, Bool())))

  val exec_conv :: post_process :: Nil = Enum(2)
  val runningState = RegInit(exec_conv)
  val sendBitsDone = RegInit(false.B)
  val sendNextPixelDone = RegInit(false.B)

  val isBufferLeft = RegInit(false.B)
  val isBufferRight = RegInit(false.B)
  val isBufferTopLeft = RegInit(false.B)
  val isBufferBottomRight = RegInit(false.B)

  compile()

  override protected def executionState(): Unit = {
    val bitLength = inputC * kernelH * kernelW
    val countLength = requiredLength(bitLength)
    val bitCounters = Seq.fill(weightsPerApply)(Module(new BitCounter(bitLength)))
    bitCounters.foreach(counter => counter.io.in := VecInit(Seq.fill(counter.width)(false.B)))

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

    io.outData.valid := true.B
    io.outData.bits  := pixel

    when(io.outData.ready) {
      val nextIdx = weightIdx + 1.U
      weightIdx := Mux(nextIdx === countsForApplyingAllWeights.U, 0.U, nextIdx)
      when(nextIdx === countsForApplyingAllWeights.U) {

      }
    }


    when(window.io.window.valid) {
      (weightsss zip biasess).zipWithIndex.foreach {
        case ((weightss, biases), idx) =>
          when(weightIdx === idx.U) {
            val activatedBits = (weightss, biases, bitCounters).zipped.map {
              case (weights, bias, bitCounter) =>
                val bits = (weights zip window.io.window.bits).flatMap {
                  case (weight, pixels) => pixels.bits.map(_ ^ weight.asBool())
                }

                bitCounter.io.in := VecInit(bits)
                bitCounter.io.count > bias.U
            }

            isBufferLeft := window.io.isLeft
            isBufferRight := window.io.isRight
            isBufferTopLeft := window.io.isTopLeft
            isBufferBottomRight := window.io.isBottomRight
            activatedBuffer(idx.U) := VecInit(activatedBits)
          }
      }

      val nextIdx = weightIdx + 1.U
      weightIdx := Mux(nextIdx === countsForApplyingAllWeights.U, 0.U, nextIdx)
      when(nextIdx === countsForApplyingAllWeights.U) {
        runningState := post_process
        sendBitsDone := false.B
        sendNextPixelDone := false.B
      }
    }

    val readyForNextPixel = ((nextInputBufferIdx === stride.U) & io.inData.valid) | isInputBufferFull
    when(!window.io.window.valid & readyForNextPixel) {
      val reachBottomRight = nextPixelBits.foldLeft(false.B) { case (acc, pixel) => acc | pixel.bottomRight }
      shiftPolicy := Mux(reachBottomRight, force_shift, shiftPolicy)
      window.io.nextPixel.valid := true.B
      inputBufferIdx := 0.U
    }
  }
}
