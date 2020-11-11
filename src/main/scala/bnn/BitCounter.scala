package bnn

import chisel3._
import chisel3.util.Cat

import scala.annotation.tailrec
import scala.math.{floor, log10}

class BitCounter(val width: Int) extends Module {
  assert(width > 0, s"width must be larger than 0, but $width")

  def atLeastWidth(n: Int): Int = {
    val logged = log10(n) / log10(2)
    floor(logged).toInt + 1
  }

  def feedToCSA(bits: Vector[Bool]): (Vector[Bool], Vector[Bool]) = {
    def onlyOneBit: (Vector[Bool], Vector[Bool]) = (bits, Vector.empty)
    def onlyTwoBits: (Vector[Bool], Vector[Bool]) = {
      val ha = Module(new HA)
      ha.io.a := bits(0)
      ha.io.b := bits(1)

      val out = Vector(ha.io.out)
      val cout = Vector(ha.io.cout)

      (out, cout)
    }

    def moreThanThreeBits: (Vector[Bool], Vector[Bool]) = {
      val parts = bits.sliding(3, 3).toVector
      val isSplit = parts.last.length < 3
      val subjects =
        if(isSplit) parts.init
        else parts

      val pairs = subjects.map { bits =>
        val csa = Module(new CSA)
        (csa.io.in zip bits).foreach { case (port, bit) => port := bit }

        (csa.io.out, csa.io.cout)
      }

      val (outs, carries) = pairs.unzip
      val nexts =
        if(isSplit) parts.last ++ outs
        else outs

      (nexts, carries)
    }

    bits.length match {
      case 0 => throw new Exception("length is 0")
      case 1 => onlyOneBit
      case 2 => onlyTwoBits
      case _ => moreThanThreeBits
    }
  }

  def convolution(bits: Vector[Bool]): UInt = {
    def allCat(bits: Vector[Bool]): UInt = {
      if(bits.length == 1) bits.head.asUInt()
      else bits.init.reverse.foldLeft(bits.last.asUInt()){ case (acc, bit) => Cat(bit, acc) }
    }

    def loop(bitss: Vector[Vector[Bool]]): UInt = {
      val (outss, carriess) = bitss.map(bits => feedToCSA(bits)).unzip

      val lsbs = outss.last
      val lsbss = carriess.tail
      val msbs = carriess.head
      val msbss = outss.init

      val summarized = (lsbss zip msbss).map{ case (a, b) => a ++ b}
      val nexts =
        if(msbs.isEmpty) summarized :+ lsbs
        else msbs +: summarized :+ lsbs
      val isContinue = nexts.exists(_.length != 1)
      val result =
        if(isContinue) loop(nexts)
        else allCat(nexts.flatten)

      result
    }

    loop(Vector(bits))
  }

  val io = IO(new Bundle{
    val in = Input(Vec(width, Bool()))
    val count = Output(UInt(atLeastWidth(width).W))
  })

  io.count := convolution(io.in.toVector)
}

class CSA extends Module {
  val io = IO(new Bundle {
    val in = Input(Vec(3, Bool()))
    val out = Output(Bool())
    val cout = Output(Bool())
  })

  val ha = Vector.fill(2)(Module(new HA))
  ha(0).io.a := io.in(0)
  ha(0).io.b := io.in(1)
  ha(1).io.a := ha(0).io.out
  ha(1).io.b := io.in(2)

  io.out := ha(1).io.out
  io.cout := ha(0).io.cout | ha(1).io.cout
}

class HA extends Module {
  val io = IO(new Bundle {
    val a = Input(Bool())
    val b = Input(Bool())
    val out = Output(Bool())
    val cout = Output(Bool())
  })

  io.out := io.a ^ io.b
  io.cout := io.a & io.b
}


