package bnn

import chisel3._

class SyncReadMemOnePort[T <: Data](size: Int, gen: T) extends Module {
  val addrData = UInt(requiredLength(size).W)

  val io = IO(new Bundle {
    val raddr = Input(addrData)
    val rdata = Output(gen)

    val wen   = Input(Bool())
    val waddr = Input(addrData)
    val wdata = Input(gen)
  })

  val mem = SyncReadMem(size, gen, SyncReadMem.ReadFirst)

  def read(addr: UInt): T = {
    io.raddr := addr
    io.rdata
  }

  def write(addr: UInt, data: T): Unit = {
    io.wen := true.B
    io.waddr := addr
    io.wdata := data
  }

  io.rdata := mem.read(io.raddr)
  when(io.wen) {
    mem.write(io.waddr, io.wdata)
  }
}

object SyncReadMemOnePort {
  def apply[T <: Data](size: Int, gen: T): SyncReadMemOnePort[T] = {
    val module = Module(new SyncReadMemOnePort[T](size, gen))

    module.io.raddr := DontCare

    module.io.wen := false.B
    module.io.waddr := DontCare
    module.io.wdata := DontCare

    module
  }
}
