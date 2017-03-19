package com.poslegm.scalaphash

import java.awt.image.BufferedImage

import com.poslegm.scalaphash.ImageUtils._

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object PHash {
  private lazy val Theta180 = Array.tabulate(180)(_ * Math.PI/180)
  private lazy val TanTheta180 = Array.tabulate(180)(i => Math.tan(Theta180(i)))
  
  def dctHash(image: BufferedImage): Future[Long] = {
    for {
      processedImage <- image.makeGrayScale().makeConvolved()
    } yield {

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

  private def createDctMatrix(size: Int): Array[Array[Float]] = {
    val c = Math.sqrt(2.0 / size).toFloat
    Array.tabulate(size, size) {
      case (_, 0) => 1 / Math.sqrt(size).toFloat
      case (x, y) => c * Math.cos((Math.PI / 2 / size) * y * (2 * x + 1)).toFloat
    }
  }

  // TODO resize CUBIC
  def marrHash(image: BufferedImage, alpha: Int = 2, level: Int = 1): Future[Array[Int]] = {
    val processed = image.makeGrayScale().makeBlurred(1).resize(512,512).equalize(256)

    val kernel = createMarrKernel(alpha, level)

    processed.correlate(kernel).map { fresp =>
      val normalized = fresp.normalize(0, 1)
      val blocks = Array.tabulate(31, 31) {
        case (x, y) => normalized.getRGB(x * 16, y * 16, 16, 16, null, 0, 16).sum.toFloat
      }

      var hashByte, onesCount, zerosCount, bitIndex = 0
      val hash = ArrayBuffer.fill(72)(0)

      for (i <- 0 until 29 by 4; j <- 0 until 29 by 4) {
        val subsec = blocks.crop(j, i, j + 2, i + 2).flatten
        val ave = subsec.sum / subsec.length

        subsec.foreach { s =>
          hashByte <<= 1
          if (s > ave) {
            hashByte |= 1
            onesCount += 1
          } else {
            zerosCount += 1
          }

          bitIndex += 1

          if ((bitIndex % 8) == 0) {
            hash((bitIndex / 8) - 1) = hashByte
            hashByte = 0
          }
        }
      }

      hash.toArray
    }
  }

  def marrHashDistance(hash1: Array[Int], hash2: Array[Int]): Option[Double] = {
    if (hash1.length != hash2.length || hash1.isEmpty) {
      None
    } else {
      val distance = hash1.toSeq.zip(hash2).foldLeft(0.0) {
        case (dist, (byte1, byte2)) => dist + bitCount(byte1 ^ byte2)
      }
      val maxBitsCount = hash1.length * 8
      Some(distance / maxBitsCount)
    }
  }

  def radialHash(image: BufferedImage, sigma: Int = 1, gamma: Int = 1, projectionsCount: Int = 180): Array[Int] = {
    val grayscaled = if (image.getColorModel.getColorSpace.getNumComponents >= 3) {
      image.makeGrayScale()
    } else {
      image
    }

    val blurred = grayscaled.makeBlurred(sigma)

    val processed = (blurred / blurred.max).pow(gamma)
    val projections = calculate180Projections(image, projectionsCount)
    val features = calculateFeatures(projections)

    calculateHash(features)
  }

  def radialHashDistance(hash1: Array[Int], hash2: Array[Int]): Double = {
    val meanX: Double = hash1.sum / hash2.length
    val meanY: Double = hash2.sum / hash2.length
    var max = 0.0
    for (d <- hash2 .indices) {
      var num = 0.0
      var denX = 0.0
      var denY = 0.0
      for (i <- hash2 .indices) {
        val hash2Index = (hash2 .length + i - d) % hash2.length
        num  += (hash1(i) - meanX) * (hash2(hash2Index) - meanY)
        denX += Math.pow(hash1(i) - meanX, 2)
        denY += Math.pow(hash2(hash2Index) - meanY, 2)
      }
      max = Math.max(max, num / Math.sqrt(denX * denY))
    }
    max
  }

  private def calculate180Projections(image: BufferedImage, projectionsCount: Int): Projections = {
    val maxSide = if (image.getWidth > image.getHeight) image.getWidth else  image.getHeight
    val x_off = (image.getWidth >> 1) + (image.getWidth & 0x1) // round(image.getWidth/2) but only with integer operations
    val y_off = (image.getHeight >> 1) + (image.getHeight & 0x1) // round(image.getHeight/2) but only with integer operations

    val radonMap = Array.fill(projectionsCount, maxSide)(0)
    val countPerLine = Array.fill(projectionsCount)(0)

    for {
      k <- 0 until (projectionsCount / 4 + 1)
      alpha = TanTheta180(k)
      x <- 0 until maxSide
    } {
      val y = alpha * (x - x_off)
      val yd = Math.floor(y + (if (y >= 0) 0.5 else -0.5)).toInt
      if ((yd + y_off >= 0) && (yd + y_off < image.getHeight) && (x < image.getWidth)) {
        radonMap(k)(x) = image.getY(x, yd + y_off)
        countPerLine(k) += 1
      }
      if ((yd + x_off >= 0) && (yd + x_off < image.getWidth) && (k != projectionsCount/4) && (x < image.getHeight)) {
        radonMap(projectionsCount / 2 - k)(x) = image.getY(yd + x_off, x)
        countPerLine(projectionsCount / 2 - k) += 1
      }
    }

    var j= 0
    for (k <- (3 * projectionsCount / 4) until projectionsCount) {
      val alpha = TanTheta180(k)
      for (x <- 0 until maxSide) {
        val y = alpha * (x - x_off)
        val yd = Math.floor(y + (if (y >= 0) 0.5 else -0.5)).toInt
        if ((yd + y_off >= 0) && (yd + y_off < image.getHeight) && (x < image.getWidth)) {
          radonMap(k)(x) = image.getY(x, yd + y_off)
          countPerLine(k) += 1
        }
        if ((y_off - yd >= 0)
          && (y_off - yd < image.getWidth)
          && (2 * y_off - x >= 0)
          && (2 * y_off - x < image.getHeight) && (k != 3 * projectionsCount / 4)) {
          radonMap(k - j)(x) = image.getY(-yd + y_off, -(x - y_off) + y_off)
          countPerLine(k - j) += 1
        }
      }
      j += 2
    }

    Projections(countPerLine, radonMap)
  }

  private def calculateFeatures(projections: Projections): Array[Double] = {
    val features = Array.fill(projections.projectionsCount)(0)

    var featuresSum = 0.0
    var featuresSumSqd = 0.0

    for (k <- 0 until projections.projectionsCount) {
      val pixelsCount = projections.countPerLine(k)
      val (lineSum, lineSumSqd) = (0 until projections.maxDimension).foldLeft((0, 0)) {
        case ((s, sumSqd), i) => (
          s + projections.projections(k)(i),
          sumSqd + projections.projections(k)(i) * projections.projections(k)(i)
        )
      }
      features(k) = (lineSumSqd / pixelsCount) - (lineSum * lineSum) / (pixelsCount * pixelsCount)
      featuresSum += features(k)
      featuresSumSqd += features(k) * features(k)
    }
    val mean = featuresSum / projections.projectionsCount
    val meanSqd = (featuresSum * featuresSum) / (projections.projectionsCount * projections.projectionsCount)
    val x = Math.sqrt((featuresSumSqd / projections.projectionsCount) - meanSqd)

    features.map(f => (f - mean) / x)
  }

  private def calculateHash(features: Array[Double]): Array[Int] = {
    val coeffsCount = 40

    var max = 0.0
    var min = Double.MaxValue
    val digest = Array.tabulate(coeffsCount) { k =>
      val sum = features.indices.foldLeft(0.0) { (acc, i) =>
        acc + features(i) * Math.cos((Math.PI * (2 * i + 1) * k) / (2 * features.length))
      }

      val value = if (k == 0) {
        sum / Math.sqrt(features.length)
      } else {
        sum * Math.sqrt(2) / Math.sqrt(features.length)
      }

      if (value > max) { max = value }
      if (value < min) { min = value }

      value
    }

    digest.map(d => (255 * (d - min) / (max - min)).toInt)
  }

  private def bitCount(x: Int): Int = {
    @tailrec
    def iter(x: Int, num: Int = 0): Int = x match {
      case 0 => num
      case _ => iter(x & (x - 1), num + 1)
    }
    iter(x)
  }

  private def createMarrKernel(alpha: Int, level: Int): FloatMatrix = {
    val sigma = 4 * Math.pow(alpha, level).toInt
    Array.tabulate(2 * sigma + 1, 2 * sigma + 1) { (x, y) =>
      val xPos = Math.pow(alpha, -level) * (x - sigma)
      val yPos = Math.pow(alpha, -level) * (y - sigma)
      val A = xPos * xPos + yPos * yPos
      (2 - A.toFloat) * Math.exp(-A / 2).toFloat;
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

    /**
      * Crops matrix from x1 to x2 and y1 to y2 (inclusive)
      * */
    def crop(x1: Int, y1: Int, x2: Int, y2: Int): FloatMatrix = {
      Array.tabulate(x2 - x1 + 1, y2 - y1 + 1)((i, j) => matrix(i + x1)(j + y1))
    }
  }
}

case class Projections(countPerLine: Array[Int], projections: Array[Array[Int]]) {
  def projectionsCount: Int = countPerLine.length
  def maxDimension: Int = projections(0).length
}