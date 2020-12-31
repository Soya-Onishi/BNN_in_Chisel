package bnn

import chisel3._

class Pixel[T <: Data](gen: T) extends Bundle {
  val bits = gen
  val valid = Bool()
  val topLeft = Bool()
  val bottomRight = Bool()
  val left = Bool()
  val right = Bool()

  override def cloneType: this.type = new Pixel[T](gen.cloneType).asInstanceOf[this.type]
}

object Pixel {
  def apply[T <: Data](gen: T) = new Pixel[T](gen)
  def invalid[T <: Data](gen: T): Pixel[T] = {
    val p = Wire(new Pixel[T](gen))
    p.bits        := DontCare
    p.valid       := false.B
    p.topLeft     := false.B
    p.bottomRight := false.B
    p.left        := false.B
    p.right       := false.B

    p
  }
}
