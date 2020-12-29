package bnn

import chisel3._
import chisel3.util._

class BinaryActivation2(
  val size: Int,
  val inputWidth: Int,
  val biases: Seq[Int]
) extends Module {
  val io = IO(new Bundle {
    val inData = Flipped(DecoupledIO(Vec(size, UInt(inputWidth.W))))
    val outData = DecoupledIO(Vec(size, Bool()))
  })
  val biasIdxMax = math.ceil(biases.length.toFloat / size.toFloat).toInt - 1
  val biasWidth = unsignedBitLength(biases.max)

  val (biasCounter, biasIdx) = DeluxeCounter(biasIdxMax)
  val biasess = VecInit(biases.map(_.U(biasWidth.W)).sliding(size, size).map(v => VecInit(v)).toSeq)
  val activateds = (io.inData.bits zip biasess(biasIdx.current)).map{ case (v, bias) => v > bias }

  when(io.inData.valid && io.outData.ready) {
    biasCounter.count()
  }

  io.outData.bits := VecInit(activateds)
  io.outData.valid := io.inData.valid
  io.inData.ready := io.outData.ready
}

abstract class BinaryActivation[InputType <: Data, OutputType <: Data](
  channels: Int,
  inputSize: Int,
  inputWidth: Int,
  biases: Seq[Int],
  inputType: InputType,
  outputType: OutputType
) extends BinaryLayer {
  assert(inputSize <= channels)
  val countsForAllChannels = math.ceil(channels.toFloat / inputSize.toFloat).toInt

  val io = IO(new Bundle {
    val inData = Flipped(DecoupledIO(inputType))
    val outData = DecoupledIO(outputType)
  })

  val receiving :: activation :: sending :: Nil = Enum(3)
  val globalState = RegInit(receiving)
  val biasIdx = RegInit(0.U(requiredLength(countsForAllChannels).W))
  val inputBuffer = Reg(Valid(Vec(inputSize, UInt(inputWidth.W))))
  val outputBuffer = Reg(Vec(countsForAllChannels, Vec(inputSize, Bool())))

  val biasess = VecInit(biases.sliding(inputSize, inputSize).toSeq.map(biases => VecInit(biases.map(_.asUInt(inputWidth.W)))))
  val nextIdxTmp = biasIdx + 1.U
  val nextIdx = WireInit(Mux(
    globalState === activation,
    Mux(nextIdxTmp === countsForAllChannels.U, 0.U, nextIdxTmp),
    biasIdx
  ))

  protected def renewBuffer(): Unit

  protected def receivingState(): Unit = {
    when(io.inData.valid) {
      globalState := activation
      inputBuffer.valid := true.B
    }
  }

  protected def activationState(): Unit = {
    val activated = (inputBuffer.bits zip biasess(biasIdx)).map{ case (in, bias) => in > bias }
    val activatedVec = VecInit(activated)

    outputBuffer(biasIdx) := activatedVec
    biasIdx               := nextIdx
    inputBuffer.valid     := io.inData.valid

    when(nextIdx === 0.U) {
      globalState := sending
    } .otherwise {
      globalState := Mux(io.inData.valid, activation, receiving)
    }
  }

  protected def sendingState(): Unit = {
    io.outData.valid := true.B

    when(io.outData.ready) {
      globalState := Mux(inputBuffer.valid, activation, receiving)
    }
  }

  protected def elaborate(): Unit = {
    renewBuffer()

    switch(globalState) {
      is(receiving) { receivingState() }
      is(activation) { activationState() }
      is(sending) { sendingState() }
    }
  }
}
