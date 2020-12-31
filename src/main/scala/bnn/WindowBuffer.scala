package bnn

import chisel3._
import chisel3.util._

class WindowBuffer[T <: Data](gen: T, inputSize: (Int, Int), kernelSize: (Int, Int), stride: Int) extends Module {
  val (inputH, inputW) = inputSize
  val (kernelH, kernelW) = kernelSize
  val xMaxApplyCount = (inputW - kernelW) / stride + 1
  val yMaxApplyCount = (inputH - kernelH) / stride + 1
  val memDepth = xMaxApplyCount - 1

  val io = IO(new Bundle {
    val window = ValidIO(Vec(kernelH * kernelW, Pixel(gen)))
    val isLeft = Output(Bool())
    val isRight = Output(Bool())
    val isTopLeft = Output(Bool())
    val isBottomRight = Output(Bool())
    val nextPixel = Flipped(ValidIO(Vec(stride, Pixel(gen))))

    val forceShift = Input(Bool())
    val isInit = Output(Bool())
  })

  val init :: fill_pixel :: output_window :: vertical_stride :: Nil = Enum(4)
  val globalState = RegInit(init)

  val initPixel = WireInit(Pixel(gen), DontCare)
  initPixel.valid       := false.B
  initPixel.topLeft     := false.B
  initPixel.bottomRight := false.B
  initPixel.left        := false.B
  initPixel.right       := false.B

  val windowH = kernelH * stride
  val windowW = math.ceil(kernelW.toFloat / stride.toFloat).toInt
  val window = RegInit(
    VecInit(Seq.fill(windowH)(
      VecInit(Seq.fill(windowW)(initPixel)))
    )
  )

  val buffersOpt =
    if(memDepth > 0) Some(Vector.fill(windowH)(SyncReadMemOnePort(memDepth, Pixel[T](gen))))
    else             None
  val memIdx = RegInit(0.U(requiredLength(memDepth).W))
  val xstrideCount = RegInit(0.U(requiredLength(xMaxApplyCount).W))
  val ystrideCount = RegInit(0.U(requiredLength(yMaxApplyCount).W))
  val leftAtLeft = WireInit(false.B)
  val topLeftReady = WireInit(false.B)

  val vertStrideCount = RegInit(0.U(requiredLength(stride).W))
  val initMemIdx = RegInit(0.U(requiredLength(memDepth).W))
  val counter = dontTouch(RegInit(0.U(32.W)))

  io.isInit        := globalState === init
  io.window.valid  := globalState === output_window
  io.window.bits   := VecInit(window.sliding(stride, stride).toSeq.flatMap(arr => arr.transpose.flatten.take(kernelW)))
  io.isLeft        := xstrideCount === 0.U
  io.isRight       := xstrideCount === (xMaxApplyCount - 1).U
  io.isTopLeft     := (xstrideCount === 0.U) & (ystrideCount === 0.U)
  io.isBottomRight := (xstrideCount === (xMaxApplyCount - 1).U) & (ystrideCount === (yMaxApplyCount - 1).U)

  val requireStride = dontTouch(WireInit(io.nextPixel.valid | io.forceShift))
  val nextIdxCalc = memIdx + 1.U
  val nextIdx = dontTouch(WireInit(Mux(nextIdxCalc === memDepth.U, 0.U, nextIdxCalc)))
  val readIdx = dontTouch(WireInit(Mux(requireStride, nextIdx, memIdx)))
  val bufferElemsOpt = buffersOpt.map(_.map(buf => buf.read(readIdx)))
  when(requireStride) {
    memIdx := nextIdx

    val partitioned = window.sliding(stride, stride).toSeq
    val heads = WireInit(VecInit(partitioned.map(_.map(_.head)).tail.flatten))

    val lasts = bufferElemsOpt.getOrElse(heads ++ io.nextPixel.bits)
    val tails = partitioned.flatMap(_.map(_.tail))
    val nextWindow = (tails zip lasts).map{ case (row, l) => row :+ l }.map(row => VecInit(row))
    window := VecInit(nextWindow)

    buffersOpt.foreach { buffers =>
      val last = Mux(io.nextPixel.valid, io.nextPixel.bits, VecInit(Seq.fill(stride)(initPixel)))
      val nextLasts = heads ++ last
      (nextLasts zip buffers).foreach{ case (head, buffer) => buffer.write(memIdx, head) }
    }

    val isLeft = nextWindow.sliding(stride, stride).foldLeft(false.B) {
      case (acc, row) => acc | (row.head.head.left & row.head.head.valid)
    }

    leftAtLeft := isLeft
    topLeftReady := nextWindow.head.head.topLeft & nextWindow.head.head.valid
  }

  switch(globalState) {
    is(init)            { initialize() }
    is(fill_pixel)      { fillPixel() }
    is(output_window)   { outputWindow() }
    is(vertical_stride) { verticalStride() }
  }

  private def initialize(): Unit = {
    buffersOpt match {
      case None => globalState := fill_pixel
      case Some(buffers) =>
        val nextIdx = initMemIdx + 1.U
        initMemIdx := Mux(nextIdx === memDepth.U, 0.U, nextIdx)

        buffers.foreach(buf => buf.write(initMemIdx, initPixel))

        when(nextIdx === memDepth.U) {
          globalState := fill_pixel
        }
    }
  }

  private def fillPixel(): Unit = {
    xstrideCount := 0.U
    ystrideCount := 0.U

    when(requireStride & topLeftReady) {
      globalState := output_window
    }
  }

  private def outputWindow(): Unit = {
    when(requireStride) {
      val nextCount = xstrideCount + 1.U
      val isRight = nextCount === xMaxApplyCount.U
      xstrideCount := Mux(nextCount === xMaxApplyCount.U, 0.U, nextCount)

      when(isRight) {
        globalState := Mux(ystrideCount === (yMaxApplyCount - 1).U, fill_pixel, vertical_stride)
      }
    }
  }

  private def verticalStride(): Unit = {
    val nextCount = vertStrideCount + 1.U
    when(leftAtLeft & requireStride) {
      vertStrideCount := Mux(nextCount === stride.U, 0.U, nextCount)

      when(nextCount === stride.U) {
        ystrideCount := ystrideCount + 1.U
        globalState := output_window
      }
    }
  }
}
