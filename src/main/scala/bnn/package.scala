import scala.math

package object bnn {
  def requiredLength(maxValue: Int): Int = {
    val log2 = math.log(maxValue) / math.log(2)
    math.max(1, math.ceil(log2).toInt)
  }
}
