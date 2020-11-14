package bnn

import chisel3._
import chisel3.util._

abstract class Binary2DLayer(
  kernelSize: (Int, Int),
  inputSize: (Int, Int, Int),
  outputChannel: Int,
  stride: Int
) extends Module {
  val (kernelH, kernelW) = kernelSize
  val (inputH, inputW, inputC) = inputSize
  val windowW = math.ceil(kernelW.toFloat / stride.toFloat).toInt
  val windowH = kernelH * stride

  require(stride <= kernelH)
  require(stride <= kernelW)

  val io = IO(new Bundle {
    val inData = Flipped(Decoupled(Pixel(Vec(inputC, Bool()))))
    val outData = Decoupled(Pixel(Vec(outputChannel, Bool())))
    val isInit = Output(Bool())
  })

  val init :: wait_executable :: execute_state :: Nil = Enum(3)
  val wait_to_ready :: force_shift :: Nil = Enum(2)
  val globalState = RegInit(init)
  val shiftPolicy = RegInit(wait_to_ready)

  val inputBuffers = Reg(Vec(stride, Pixel(Vec(inputC, Bool()))))
  val inputBufferIdx = RegInit(0.U(requiredLength(stride).W))
  val isInputBufferFull = WireInit(inputBufferIdx === stride.U)
  val nextInputBufferIdx = WireInit(inputBufferIdx + 1.U)
  val reachBottomRight = RegInit(false.B)

  io.inData.ready             := !isInputBufferFull
  io.outData.valid            := false.B
  io.outData.bits.bits        := DontCare
  io.outData.bits.valid       := false.B
  io.outData.bits.left        := false.B
  io.outData.bits.right       := false.B
  io.outData.bits.topLeft     := false.B
  io.outData.bits.bottomRight := false.B
  io.isInit                   := globalState === init

  val window = Module(new WindowBuffer(Vec(inputC, Bool()), (inputH, inputW), kernelSize, stride))
  val nextPixelBits = Mux(
    io.inData.valid & nextInputBufferIdx === stride.U,
    VecInit(inputBuffers.init :+ io.inData.bits),
    inputBuffers
  )

  window.io.forceShift := false.B
  window.io.nextPixel.valid := false.B
  window.io.nextPixel.bits := nextPixelBits

  when(io.inData.valid & !isInputBufferFull & globalState =/= init) {
    inputBuffers(inputBufferIdx) := io.inData.bits
    inputBufferIdx := nextInputBufferIdx
  }

  protected def compile(): Unit = {
    switch(globalState) {
      is(init) {
        initState()
      }
      is(wait_executable) {
        waitToExecute()
      }
      is(execute_state) {
        executionState()
      }
    }
  }

  private def initState(): Unit = {
    when(!window.io.isInit) {
      globalState := wait_executable
    }
  }

  private def waitToExecute(): Unit = {
    when(window.io.window.valid){
      shiftPolicy := wait_to_ready
      globalState := execute_state
    } .otherwise {
      val readyInput = (io.inData.valid & nextInputBufferIdx === stride.U) | isInputBufferFull

      when(readyInput) {
        window.io.nextPixel.valid := true.B
        inputBufferIdx := 0.U
      }
    }
  }

  protected def executionState(): Unit
}
