package bnn

import chisel3.stage.ChiselGeneratorAnnotation
import scala.util.Random

object BNNMain extends App {
  val kernelH = 3
  val kernelW = 3
  val stride = 1
  val weightNum = 9
  val cycles = 3
  val inputShape = (48, 48, 3)

  val rnd = new Random(0)
  val kernelSize = (kernelH, kernelW)
  val weightss = Seq.fill(weightNum)(Seq.fill(kernelH * kernelW)(rnd.nextBoolean()))
  val biases = Seq.fill(weightNum)(rnd.nextInt(255))

  val annon = ChiselGeneratorAnnotation(() =>
    new BinaryConv2D(
      kernelSize = (3, 3),
      weights = weightss,
      bias = biases,
      inputShape = inputShape,
      cyclesForAllWeights = cycles,
      stride = stride
    )
  )

  (new chisel3.stage.ChiselStage).execute(
    Array("-X", "verilog"),
    Seq(annon)
  )
}
