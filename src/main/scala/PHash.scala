import java.awt.image.BufferedImage

import ImageUtils._

object PHash {
  def dctHash(image: BufferedImage): Long = {
    val processedImage = image.makeGrayScale().makeConvolved()

    val matrix = processedImage.resize(32, 32).toMatrix

    val dctMatrix = createDctMatrix(32)
    val dctMatrixTransposed = dctMatrix.transpose
    val dctImage = dctMatrix * matrix * dctMatrixTransposed

    val subSec = dctImage.crop(1, 1, 8, 8).flatten

    val median = findMedian(subSec)
    (0 until 64).foldLeft(0) {
      case (res, i) if subSec(i) > median => res | (1 << i)
      case (res, _) => res
    }
  }

  def dctHashDistance(hash1: Long, hash2: Long): Long = {
    var x = hash1 ^ hash2
    val m1  = 0x5555555555555555L
    val m2  = 0x3333333333333333L
    val h01 = 0x0101010101010101L
    val m4  = 0x0f0f0f0f0f0f0f0fL
    x -= (x >> 1) & m1
    x = (x & m2) + ((x >> 2) & m2)
    x = (x + (x >> 4)) & m4
    (x * h01) >> 56
  }

  def createDctMatrix(size: Int): Array[Array[Float]] = {
    val c = Math.sqrt(2.0 / size).toFloat
    Array.tabulate(size, size) {
      case (_, 0) => 1 / Math.sqrt(size).toFloat
      case (x, y) => c * Math.cos((Math.PI / 2 / size) * y * (2 * x + 1)).toFloat
    }
  }

  private def findMedian(floats: Seq[Float]): Float = floats match {
    case xs if xs.length % 2 == 0 =>
      val tail = xs.sorted.drop(xs.length / 2 - 1)
      (tail.head + tail.tail.head) / 2.0f
    case xs => xs.sorted.drop(xs.length / 2).head
  }

  type FloatMatrix = Array[Array[Float]]

  implicit class FloatMatrixOps(matrix: FloatMatrix) {
    def * (other: FloatMatrix): FloatMatrix = {
      if (matrix.length == 0 || matrix(0).length == 0 || matrix(0).length != other.length) {
        throw new IllegalArgumentException(s"Can't multiply matrices")
      }

      Array.tabulate(matrix.length, other(0).length) { (i, j) =>
        matrix(0).indices.foldLeft(0.0f)((res, k) => res + matrix(i)(k) * other(k)(j))
      }
    }

    def crop(x1: Int, y1: Int, x2: Int, y2: Int): FloatMatrix = {
      Array.tabulate(x2 - x1 + 1, y2 - y1 + 1)((i, j) => matrix(i + x1)(j + y1))
    }
  }
}
