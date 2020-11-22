import chisel3._
import java.nio.file.{Path, Paths}

package object bnn {
  private var denseWeightID = 0

  def requiredLength(maxValue: Int): Int = {
    val log2 = math.log(maxValue + 1) / math.log(2)
    math.max(1, math.ceil(log2).toInt)
  }

  def denseWeightName(): Path = {
    val id = denseWeightID
    denseWeightID += 1
    val name = s"dense_weights_$id"

    Paths.get(name)
  }
}
