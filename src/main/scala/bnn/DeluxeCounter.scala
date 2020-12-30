package bnn

import chisel3._
import chisel3.util._
import chisel3.experimental._

class CountState(max: Int) extends Bundle {
  private val requiredWidth = unsignedBitLength(max)
  val current = UInt(requiredWidth.W)
  val next    = UInt(requiredWidth.W)
  val wrapped = Bool()

  override def cloneType: this.type = new CountState(max).asInstanceOf[this.type]
}

object CountState {
  def apply(maxValue: Int): CountState = new CountState(maxValue)
}

@chiselName
class DeluxeCounter(maxValue: Int) extends Module {
  assert(maxValue >= 0, s"require max value more than or equal 0, but $maxValue")

  val io = IO(new Bundle {
    val enable = Input(Bool())
    val state = Output(CountState(maxValue))
  })

  val r = RegInit(0.U(unsignedBitLength(maxValue).max(1).W))
  val reachMax = WireInit(r === maxValue.U)
  val nextValue = WireInit(Mux(reachMax, 0.U, r + 1.U))

  when(io.enable) {
    r := nextValue
  }

  io.state.current := r
  io.state.next    := nextValue
  io.state.wrapped := reachMax

  def count(): Unit = {
    this.io.enable := true.B
  }
}

object DeluxeCounter {
  def apply(maxValue: Int): (DeluxeCounter, CountState) = {
    val m = Module(new DeluxeCounter(maxValue))
    m.io.enable := false.B

    (m, m.io.state)
  }
}
