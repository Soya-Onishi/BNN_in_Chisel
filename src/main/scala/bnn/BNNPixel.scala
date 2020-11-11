package bnn

import chisel3._

class BNNPixel[T <: Data] private (d: T) extends Bundle {
  val data: T = d
  val isValid: Bool = Bool()
  val isLeftEdge: Bool = Bool()
  val isRightEdge: Bool = Bool()
  val isTopEdge: Bool = Bool()
  val isBottomEdge: Bool = Bool()

  override def cloneType: this.type = new BNNPixel[T](d).asInstanceOf[this.type]
}

object BNNPixel {
  def apply[T <: Data](data: T): BNNPixel[T] = new BNNPixel[T](data)
}
