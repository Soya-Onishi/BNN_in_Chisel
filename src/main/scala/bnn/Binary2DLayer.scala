package bnn

import chisel3._
import chisel3.util._
import chisel3.experimental._

@chiselName
class Binary2DLayer[InputType <: Data](
  kernelSize: (Int, Int),
  inputSize: Int,
  inputShape: (Int, Int, Int),
  stride: Int,
  inputType: InputType,
) extends BinaryLayer {
  // parameters for construction
  val (kernelH, kernelW) = kernelSize
  val (inputH, inputW, inputC) = inputShape
  val pixelVecSize = math.ceil(inputC.toFloat / inputSize.toFloat).toInt
  val outputSize = kernelH * kernelW
  val inputBufferIdxMax = stride - 1
  val pixelVecIdxMax = pixelVecSize - 1
  val horizontalIdxMax = (inputW - kernelW) / stride + 1
  val rightEdgeBufCount = (if(inputW % stride == 0) stride else inputW % stride) - 1

  assert(inputC >= inputSize)

  require(stride <= kernelH)
  require(stride <= kernelW)

  val io = IO(new Bundle {
    val inData = Flipped(DecoupledIO(Pixel(Vec(inputSize, inputType))))
    val outData = DecoupledIO(Vec(outputSize, Pixel(Vec(inputC, inputType))))
    val isLeft        = Output(Bool())
    val isRight       = Output(Bool())
    val isTopLeft     = Output(Bool())
    val isBottomRight = Output(Bool())

    val isInit = Output(Bool())
  })

  val init :: fill_buffer :: execute_state :: wait_next :: Nil = Enum(4)
  val wait_for_valid :: force_shift :: Nil = Enum(2)
  val globalState = RegInit(init)
  val shiftPolicy = RegInit(wait_for_valid)

  val inputBuffers = Reg(Vec(stride, Vec(pixelVecSize, Pixel(Vec(inputSize, inputType)))))
  val isInputBufferFull = RegInit(false.B)
  val (inputBufferIdxCounter, inputBufferIdx) = DeluxeCounter(inputBufferIdxMax)
  val (pixelVecIdxCounter, pixelVecIdx) = DeluxeCounter(pixelVecIdxMax)
  val (hIdxCounter, hIdx) = DeluxeCounter(horizontalIdxMax)
  val isRightEdge = WireInit(hIdx.wrapped)
  val readyForFeeding =
    (inputBufferIdx.wrapped & pixelVecIdx.wrapped & io.inData.valid) |
    (isRightEdge & inputBufferIdx.current === rightEdgeBufCount.U & pixelVecIdx.wrapped & io.inData.valid) |
    isInputBufferFull

  val window = Module(new WindowBuffer(Vec(inputC, inputType), (inputH, inputW), kernelSize, stride))
  val bufferLast = VecInit(inputBuffers.last.init :+ io.inData.bits)
  val bufferConcat = VecInit(inputBuffers.init :+ bufferLast)
  val nextPixelBits = Mux(io.inData.valid & !isInputBufferFull, bufferConcat, inputBuffers)
  val fedNormalPixels = nextPixelBits.map{ pixels =>
    val bits = pixels.map(_.bits).reduceLeft[Seq[InputType]]{ case (acc, bits) => acc ++ bits }
    val pixel = Wire(Pixel(Vec(inputC, inputType)))
    pixel.bits        := VecInit(bits.take(inputC))
    pixel.left        := pixels.head.left
    pixel.right       := pixels.head.right
    pixel.topLeft     := pixels.head.topLeft
    pixel.bottomRight := pixels.head.bottomRight
    pixel.valid       := pixels.head.valid

    pixel
  }

  // RE means Right Edge
  val bufRESize = rightEdgeBufCount + 1
  val bufferRELast = dontTouch(WireInit(VecInit(inputBuffers.take(bufRESize).last.init :+ io.inData.bits)))
  val bufferREConcat = dontTouch(WireInit(VecInit(inputBuffers.take(bufRESize).init :+ bufferRELast)))
  val nextPixelREBits = dontTouch(WireInit(Mux(io.inData.valid & !isInputBufferFull, bufferREConcat, VecInit(inputBuffers.take(bufRESize)))))
  val fedREInitPixels = nextPixelREBits.take(bufRESize).map{ pixels =>
    val bits = pixels.map(_.bits).reduceLeft[Seq[InputType]]{ case (acc, bits) => acc ++ bits }
    val pixel = Wire(Pixel(Vec(inputC, inputType)))
    pixel.bits        := VecInit(bits.take(inputC))
    pixel.left        := pixels.head.left
    pixel.right       := pixels.head.right
    pixel.topLeft     := pixels.head.topLeft
    pixel.bottomRight := pixels.head.bottomRight
    pixel.valid       := pixels.head.valid

    pixel
  }
  val invalidPixel = Pixel.invalid(Vec(inputC, inputType))
  val fedREPixels = fedREInitPixels ++ Seq.fill(stride - bufRESize)(invalidPixel)

  val reachBottomRight = nextPixelBits.flatten.foldLeft(false.B) { case (acc, pixel) => acc | pixel.bottomRight }
  val sentBottomRight = RegInit(false.B)

  io.inData.ready  := !isInputBufferFull & globalState =/= init
  io.outData.valid := window.io.window.valid & globalState === execute_state
  io.outData.bits  := window.io.window.bits
  io.isLeft        := window.io.isLeft
  io.isRight       := window.io.isRight
  io.isTopLeft     := window.io.isTopLeft
  io.isBottomRight := window.io.isBottomRight
  io.isInit        := globalState === init

  window.io.forceShift      := false.B
  window.io.nextPixel.valid := false.B
  window.io.nextPixel.bits  := Mux(isRightEdge, VecInit(fedREPixels), VecInit(fedNormalPixels))

  // step next index
  when(io.inData.valid & !isInputBufferFull & globalState =/= init) {
    inputBuffers(inputBufferIdx.current)(pixelVecIdx.current) := io.inData.bits

    pixelVecIdxCounter.count()
    when(pixelVecIdx.wrapped) {
      inputBufferIdxCounter.count()
      val normalFull = pixelVecIdx.wrapped & inputBufferIdx.wrapped
      val rightEdgeFull = pixelVecIdx.wrapped & (inputBufferIdx.current === rightEdgeBufCount.U) & isRightEdge
      isInputBufferFull := normalFull | rightEdgeFull
    }
  }

  when(window.io.nextPixel.valid) {
    hIdxCounter.count()
    when(isRightEdge) {
      inputBufferIdxCounter.zero()
      pixelVecIdxCounter.zero()
    }
  }

  switch(globalState) {
    is(init) { initState() }
    is(fill_buffer) { fillWindowBuffer() }
    is(execute_state) { executionState() }
    is(wait_next) { waitToNext() }
  }

  private def initState(): Unit = {
    when(!window.io.isInit) {
      globalState := fill_buffer
    }
  }

  // Feed pixels until window buffer's kernel asserts valid.
  // If asserted, convolution is began in next cycle.
  private def fillWindowBuffer(): Unit = {
    when(window.io.window.valid){
      shiftPolicy := wait_for_valid
      globalState := execute_state
    } .otherwise {
      // Waiting for filling buffer.
      // If buffer is full, feed pixels into window buffer, and reset input buffer
      // (in this case, assign 0 into index register)
      when(readyForFeeding) {
        window.io.nextPixel.valid := true.B
        isInputBufferFull := false.B
      }
    }
  }

  private def executionState(): Unit = {
    when(window.io.isBottomRight) {
      sentBottomRight := true.B
    }

    when(io.outData.ready & io.outData.valid) {
      globalState := wait_next
    }

    when(!window.io.window.valid & readyForFeeding) {
      shiftPolicy := Mux(reachBottomRight, force_shift, shiftPolicy)
      window.io.nextPixel.valid := true.B
      isInputBufferFull := false.B
    }
  }

  private def waitToNext(): Unit = {
    switch(shiftPolicy) {
      is(wait_for_valid) {
        when(readyForFeeding) {
          // If bottom right pixel is fed, transit shift policy to force shift
          // because there is a chance that this bottom right pixel is last one.
          // In this case, if shift policy is still wait for waiting valid pixel,
          // this layer will be locked eternally (and following layers are also locked because no inputs are fed).
          val reachBottomRight = nextPixelBits.flatten.foldLeft(false.B) { case (acc, pixel) => acc | pixel.bottomRight }

          window.io.nextPixel.valid := true.B
          sentBottomRight           := false.B
          globalState               := Mux(sentBottomRight, fill_buffer, execute_state)
          isInputBufferFull         := false.B
          shiftPolicy               := Mux(reachBottomRight, force_shift, wait_for_valid)
        }
      }
      is(force_shift) {
        // If next input (i.e. pixel top left flag has) is fed,
        // wait for those pixels because this means that there are still input images.
        val topLeftInBuffer = nextPixelBits.flatten.foldLeft(false.B) { case (acc, pixel) => acc | pixel.topLeft }

        window.io.forceShift := true.B
        globalState          := Mux(sentBottomRight, fill_buffer, execute_state)
        sentBottomRight      := false.B
        shiftPolicy          := Mux(topLeftInBuffer | sentBottomRight, wait_for_valid, force_shift)
      }
    }
  }
}
