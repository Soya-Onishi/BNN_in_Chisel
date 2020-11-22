package bnn

import chisel3._
import chisel3.util._
import chisel3.util.experimental.loadMemoryFromFile

import java.nio.file.Files

class BinaryDense(
  inputSize: Int,
  inputNeuron: Int,
  cyclesForAllWeights: Int,
  weightss: Seq[Seq[Boolean]],
  biases: Seq[Int]
) extends BinaryLayer {
  assert(weightss.length == biases.length)
  assert(weightss.map(_.length).forall(_ == inputNeuron))

  val inputCount = math.ceil(inputNeuron.toFloat / inputSize.toFloat).toInt
  val weightsPerCycle = math.ceil(weightss.length.toFloat / cyclesForAllWeights.toFloat).toInt
  val cyclesForApplyingAllWeights = math.ceil(weightss.length.toFloat / weightsPerCycle.toFloat).toInt
  val outputSize = weightsPerCycle

  val io = IO(new Bundle {
    val inData = Flipped(DecoupledIO(Vec(inputSize, Bool())))
    val outData = DecoupledIO(Vec(outputSize, Bool()))
    val isInit = Output(Bool())
  })

  val memDepth = math.ceil(weightss.length.toFloat / weightsPerCycle.toFloat).toInt
  val weightBufferDepth = inputCount * cyclesForApplyingAllWeights
  val bitCounters = Seq.fill(weightsPerCycle)(Module(new BitCounter(inputSize)))
  val countBuffers = Seq.fill(weightsPerCycle)(SyncReadMemOnePort(memDepth, UInt(requiredLength(inputNeuron).W)))

  val init :: dense :: activation :: Nil = Enum(3)
  val globalState = RegInit(init)


  private def transformToContinuousForm(weightsss: Seq[Seq[Seq[Boolean]]]): Seq[String] = {
    def sortVertically(stringss: Seq[Seq[String]]): Seq[String] = {
      if(stringss.exists(_.isEmpty)) Seq.empty
      else stringss.map(_.head) ++ sortVertically(stringss.map(_.tail))
    }

    val stringss = weightsss.map(_.map(_.map(w => if(w) '1' else '0').mkString))
    sortVertically(stringss)
  }

  val inputBuffer = Reg(Valid(Vec(inputSize, Bool())))
  val inputReady = !inputBuffer.valid & globalState === dense
  val inputIdx = RegInit(0.U(requiredLength(inputCount).W))
  val countBufIdx = RegInit(0.U(requiredLength(cyclesForApplyingAllWeights).W))
  val weightIdx = RegInit(0.U(requiredLength(weightBufferDepth).W))

  val weightssss = weightss.map(_.sliding(inputSize, inputSize).toSeq).sliding(weightsPerCycle, weightsPerCycle).toSeq
  val weightssssString = weightssss.map(transformToContinuousForm)
  val paths = weightssssString.map { weights =>
    val path = denseWeightName()
    Files.writeString(path, weights.mkString("\n"))
    path
  }
  val weightBuffers = paths.map(path => SyncReadMemOnePort(weightBufferDepth, Vec(inputSize, Bool()), path))

  val weightIdxIncTmp = weightIdx + 1.U
  val weightIdxInc = Mux(weightIdxIncTmp === weightBufferDepth.U, 0.U, weightIdxIncTmp)
  val countBufferReadIdx = Mux(inputBuffer.valid, weightIdx + 1.U, weightIdx)
  val counts = countBuffers.map(buf => buf.read(countBufferReadIdx))
  val allCounts = Seq.fill(weightsPerCycle)(WireInit(0.U(requiredLength(inputNeuron).W)))
  val countBufIdxIncTmp = countBufIdx + 1.U
  val countBufIdxInc = Mux(countBufIdxIncTmp === cyclesForApplyingAllWeights.U, 0.U, countBufIdxIncTmp)

  val biasess = biases.sliding(weightsPerCycle, weightsPerCycle).toSeq
  val biasessVec = VecInit(biasess.map(biases => VecInit(biases.map(_.asUInt()))))

  io.outData.valid := globalState === activation
  io.outData.bits  := DontCare
  io.inData.ready  := inputReady
  io.isInit        := globalState === init
  bitCounters.foreach(c => c.io.in := DontCare)

  when(io.inData.valid & inputReady) {
    countBuffers.foreach(buf => buf.read(0.U))
    weightBuffers.foreach(buf => buf.read(0.U))
    inputBuffer.valid := true.B
    inputBuffer.bits := io.inData.bits
  }

  when(inputBuffer.valid & globalState === dense) {
    val weightss = VecInit(weightBuffers.map(_.io.rdata))
    (weightss, countBuffers, bitCounters).zipped.foreach {
      case (weights, buf, counter) =>
        val applied = inputBuffer.bits.asUInt() ^ weights.asUInt()
        counter.io.in := VecInit(applied.asBools())
        val acc = counter.io.count + buf.io.rdata
        buf.write(countBufIdx, acc)
    }

    weightBuffers.foreach(buf => buf.read(weightIdxInc))
    countBuffers.foreach(buf => buf.read(countBufIdxInc))
    weightIdx := weightIdxInc
    countBufIdx := countBufIdxInc

    when(countBufIdxIncTmp === cyclesForApplyingAllWeights.U) {
      inputBuffer.valid := false.B

      val inputIdxIncTmp = inputIdx + 1.U
      val inputIdxInc = Mux(inputIdxIncTmp === inputCount.U, 0.U, inputIdxIncTmp)
      inputIdx := inputIdxInc
      when(inputIdxIncTmp === inputCount.U) {
        globalState := activation
      }
    }
  }

  when(globalState === activation) {
    val sums = countBuffers.map(_.io.rdata)
    val readIdx = Mux(io.outData.ready, weightIdxInc, weightIdx)
    countBuffers.foreach(_.read(readIdx))
    val activateds = (sums zip biasessVec(weightIdx)).map{ case (sum, bias) => sum > bias }
    io.outData.bits := VecInit(activateds)

    when(io.outData.ready) {
      weightIdx := weightIdxInc

      when(weightIdxIncTmp === cyclesForApplyingAllWeights.U) {
        globalState := dense
      }
    }
  }
}
