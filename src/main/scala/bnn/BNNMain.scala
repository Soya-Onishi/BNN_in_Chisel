package bnn

import chisel3._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}

import scala.util.Random

object BNNMain extends App {
  val kernelH = 3
  val kernelW = 3
  val stride = 1
  val weightNum = 64
  val cycles = 8
  val inputC = 32
  val inputShape = (23, 23, inputC)

  val rnd = new Random(0)
  val kernelSize = (kernelH, kernelW)
  val weightss = Seq.fill(weightNum)(Seq.fill(kernelH * kernelW)(rnd.nextBoolean()))
  val biases = weightss.map(weights => rnd.nextInt(weights.count(identity) * inputC))

  val simplifyMem = firrtl.stage.RunFirrtlTransformAnnotation(new firrtl.transforms.SimplifyMems)
  val generator = ChiselGeneratorAnnotation(() =>
    new BinaryConv2D(
      kernelSize = (3, 3),
      weights = weightss,
      bias = biases,
      inputShape = inputShape,
      countsForAllWeights = cycles,
      stride = stride
    )
  )

  val annons = Seq(simplifyMem, generator)

  (new chisel3.stage.ChiselStage).execute(
    Array("-X", "verilog"),
    annons
  )
}

object MaxPoolingGen extends App {
  val kernelH = 3
  val kernelW = 3
  val stride = 1
  val inputC = 32
  val inputShape = (23, 23, inputC)

  val annon = ChiselGeneratorAnnotation(() =>
    new BinaryMaxPooling2D(
      kernelSize = (3, 3),
      inputSize = inputShape,
      stride = stride
    )
  )

  (new ChiselStage).execute(
    Array("-X", "verilog"),
    Seq(annon)
  )
}

object WindowBufferGen extends App {
  val kernelH = 3
  val kernelW = 3
  val kernelSize = (kernelH, kernelW)
  val stride = 1
  val weightNum = 64
  val cycles = 8
  val inputShape = (23, 23)

  val data = Vec(32, Bool())

  val annon = ChiselGeneratorAnnotation(() =>
    new WindowBuffer(
      kernelSize = kernelSize,
      gen = data,
      inputSize = inputShape,
      stride = stride
    )
  )

  (new chisel3.stage.ChiselStage).execute(
    Array("-X", "verilog"),
    Seq(annon)
  )
}
