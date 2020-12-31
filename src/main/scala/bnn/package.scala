import chisel3._
import chisel3.util._
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

  implicit val pixelVecSIntActivationUtil = new ActivationUtil[Pixel[Vec[SInt]]] {
    type RetType = Pixel[Vec[Bool]]
    type BiasType = SInt

    def genRetType(size: Int): Pixel[Vec[Bool]] = Pixel(Vec(size, Bool()))
    def activation(data: Pixel[Vec[SInt]], biases: Vec[Valid[SInt]]): Pixel[Vec[Bool]] = {
      val b = (data.bits zip biases).map{ case (p, b) => (p > b.bits) & b.valid }
      val ret = Wire(Pixel(Vec(b.length, Bool())))
      ret.bits        := VecInit(b)
      ret.left        := data.left
      ret.right       := data.right
      ret.topLeft     := data.topLeft
      ret.bottomRight := data.bottomRight
      ret.valid       := data.valid

      ret
    }
    def toBiases(biases: Seq[(Boolean, Int)]): Seq[Valid[SInt]] = {
      val width = biases.map(b => signedBitLength(b._2)).max.max(1)
      biases.map {
        case (bool, bias) =>
          val valid = Wire(Valid(SInt(width.W)))
          valid.bits := bias.S(width.W)
          valid.valid := bool.B

          valid
      }
    }
  }

  implicit val pixelUIntActivationUtil = new ActivationUtil[Pixel[Vec[UInt]]] {
    type RetType = Pixel[Vec[Bool]]
    type BiasType = UInt

    def genRetType(size: Int): Pixel[Vec[Bool]] = Pixel(Vec(size, Bool()))
    def activation(data: Pixel[Vec[UInt]], biases: Vec[Valid[UInt]]): Pixel[Vec[Bool]] = {
      val b = (data.bits zip biases).map{ case (p, b) => (p > b.bits) & b.valid }
      val ret = Wire(Pixel(Vec(b.length, Bool())))
      ret.bits        := VecInit(b)
      ret.left        := data.left
      ret.right       := data.right
      ret.topLeft     := data.topLeft
      ret.bottomRight := data.bottomRight
      ret.valid       := data.valid

      ret
    }
    def toBiases(biases: Seq[(Boolean, Int)]): Seq[Valid[UInt]] = {
      val width = biases.map(b => unsignedBitLength(b._2)).max.max(1)
      biases.map {
        case (bool, bias) =>
          val valid = Wire(Valid(UInt(width.W)))
          valid.bits  := bias.U(width.W)
          valid.valid := bool.B

          valid
      }
    }
  }

  implicit val uintVecActivationUtil = new ActivationUtil[Vec[UInt]] {
    type RetType = Vec[Bool]
    type BiasType = UInt

    def genRetType(size: Int): Vec[Bool] = Vec(size, Bool())
    def activation(data: Vec[UInt], bias: Vec[Valid[UInt]]): Vec[Bool] =
      VecInit((data zip bias).map{ case (d, b) => (d > b.bits) & b.valid })
    def toBiases(biases: Seq[(Boolean, Int)]): Seq[Valid[UInt]] = {
      val width = biases.map(b => unsignedBitLength(b._2)).max.max(1)
      biases.map {
        case (bool, bias) =>
          val valid = Wire(Valid(UInt(width.W)))
          valid.bits  := bias.U(width.W)
          valid.valid := bool.B

          valid
      }
    }
  }
}
