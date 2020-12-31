package bnn

import chisel3._
import chisel3.util._
import chisel3.experimental._

@chiselName
class BinaryDense(
  val inputSize: Int,
  val inputNeuron: Int,
  _cyclesForAllWeights: Int,
  weightss: Seq[Seq[Boolean]]
) extends BinaryLayer {
  assert(weightss.map(_.length).forall(_ == inputNeuron))

  // parameters for construction of binary dense
  val inputCount = math.ceil(inputNeuron.toFloat / inputSize.toFloat).toInt
  val inputIdxMax = inputCount - 1
  val outputWidth = unsignedBitLength(weightss.head.length)
  val weightsPerCycle = math.ceil(weightss.length.toFloat / _cyclesForAllWeights.toFloat).toInt
  val cyclesForApplyingAllWeights = math.ceil(weightss.length.toFloat / weightsPerCycle.toFloat).toInt
  val weightIdxMax = cyclesForApplyingAllWeights - 1
  val outputSize = weightss.length
  val outputPacketSize = weightsPerCycle
  val memDepth =  math.ceil(outputSize.toFloat / weightsPerCycle.toFloat).toInt // == cyclesForApplyingAllWeights

  val io = IO(new Bundle {
    val inData = Flipped(DecoupledIO(Vec(inputSize, Bool())))
    val outData = DecoupledIO(Vec(outputPacketSize, UInt(outputWidth.W)))
    val isInit = Output(Bool())
  })

  val bitCounters = Seq.fill(weightsPerCycle)(Module(new BitCounter(inputSize)))
  val countBuffers = Seq.fill(weightsPerCycle)(SyncReadMemOnePort(memDepth, UInt(outputWidth.W)))

  val init :: dense :: Nil = Enum(2)
  val globalState = RegInit(init)

  val inputBuffer = Reg(Valid(Vec(inputSize, Bool())))
  val inputReady = !inputBuffer.valid & globalState === dense

  val (inputCounter, inputIdx) = DeluxeCounter(inputIdxMax)
  val (weightIdxCounter, weightIdx) = DeluxeCounter(weightIdxMax)
  val (memIdxCounter, memIdx) = DeluxeCounter(memDepth - 1)

  io.outData.valid := false.B
  io.outData.bits  := DontCare
  io.inData.ready  := inputReady
  io.isInit        := globalState === init
  bitCounters.foreach(c => c.io.in := DontCare)

  when(io.inData.valid & inputReady) {
    inputBuffer.valid := true.B
    countBuffers.map(_.read(0.U))
    inputBuffer.bits := io.inData.bits
  }

  when(globalState === init) {
    countBuffers.foreach(buf => buf.write(memIdx.current, 0.U))
    memIdxCounter.count()
    when(memIdx.wrapped) {
      globalState := dense
    }
  }

  val weightssss = weightss
    .map(_.sliding(inputSize, inputSize).toSeq)
    .sliding(weightsPerCycle, weightsPerCycle)
    .toSeq
    .map(_.transpose)
  when(inputBuffer.valid & globalState === dense) {
    val defaultBools = VecInit(Seq.fill(weightsPerCycle)(VecInit(Seq.fill(inputSize)(false.B))))
    val weightss = MuxLookup[UInt, Vec[Vec[Bool]]](weightIdx.current, defaultBools, weightssss.zipWithIndex.map {
      case (weightsss, idx) =>
        val elems = weightsss.zipWithIndex.map {
          case (weightss, idx) =>
            val i = idx.U(unsignedBitLength(inputIdxMax).W)
            val pad = Seq.fill(weightsPerCycle - weightss.length)(VecInit(Seq.fill(inputSize)(false.B)))
            val wssBase = weightss.map { weights =>
              val padSize = inputSize - weights.length
              val pad = Seq.fill(padSize)(false.B)
              val base = weights.map(_.B)

              VecInit(base ++ pad)
            }
            val ws = wssBase ++ pad

            i -> VecInit(ws)
        }

        idx.U(unsignedBitLength(weightIdxMax).W) -> MuxLookup[UInt, Vec[Vec[Bool]]](inputIdx.current, defaultBools, elems)
    })

    val counts = (weightss, countBuffers, bitCounters).zipped.map {
      case (weights, buf, counter) =>
        val applied = inputBuffer.bits.asUInt() ^ weights.asUInt()
        counter.io.in := VecInit(applied.asBools())
        counter.io.count + buf.io.rdata
    }

    val storedCounts = counts.map{ c => Mux(inputIdx.wrapped, 0.U, c) }

    io.outData.valid := inputIdx.wrapped
    io.outData.bits := VecInit(counts)

    val transitNext = WireInit((inputIdx.wrapped & io.outData.ready) | !inputIdx.wrapped)
    val nextLoadIdx = Mux(transitNext, memIdx.next, memIdx.current)
    countBuffers.foreach(buf => buf.read(nextLoadIdx))
    when(transitNext) {
      (countBuffers zip storedCounts).foreach { case (buf, count) => buf.write(memIdx.current, count) }
      memIdxCounter.count()
      weightIdxCounter.count()
    }

    when(memIdx.wrapped & io.outData.ready) {
      inputBuffer.valid := false.B
      inputCounter.count()
    }
  }
}
