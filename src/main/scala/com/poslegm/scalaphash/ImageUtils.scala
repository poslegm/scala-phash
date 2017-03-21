package com.poslegm.scalaphash

import java.awt.image._
import java.awt.{Color, Image}

import com.sksamuel.scrimage
import com.sksamuel.scrimage.filter.GaussianBlurFilter

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object ImageUtils {

  implicit class BufferedImageExtended(image: BufferedImage) {
    private lazy val scrImage = scrimage.Image.fromAwt(image)
    private lazy val flattenPixels = createFlattenPixelsArray()
    private lazy val coordinates = for {
      x <- 0 until image.getWidth()
      y <- 0 until image.getHeight()
    } yield (x, y)

    /**
      * Makes image grayscale by converting RGB to YCbCr and keeping Y channel only
      *
      * @return BufferedImage with Y channel only
      * */
    def makeGrayScale(): BufferedImage = {
      if (image.getType == BufferedImage.TYPE_BYTE_GRAY || image.getType == BufferedImage.TYPE_USHORT_GRAY) {
        return image
      }

      val newPixels = for (i <- flattenPixels.indices by 3) yield {
        val (r, g, b) = (flattenPixels(i + 2), flattenPixels(i + 1), flattenPixels(i))
        cut(((66 * r + 129 * g + 25 * b + 128) / 256.0f + 16).toInt, 0, 255)
      }
      withNewFlattenPixels(newPixels.toArray, BufferedImage.TYPE_BYTE_GRAY)
    }

    /**
      * Applies Image Convolution algorithm
      * (https://en.wikipedia.org/wiki/Kernel_(image_processing)#Convolution)
      * @param parallelism Futures count
      * @param kernelSize size of matrix
      * @return Future with new BufferedImage
      * */
    def makeConvolved(parallelism: Int, kernelSize: Int = 7): Future[BufferedImage] = {
      val kernel = Array.tabulate(kernelSize, kernelSize)((_, _) => 1.0f / (kernelSize * kernelSize))

      correlate(kernel, parallelism)
    }

    /**
      * Applies Image Correlation algorithm
      * (https://en.wikipedia.org/wiki/Digital_image_correlation)
      * @param kernel correlation matrix
      * @param parallelism Futures count
      * @return Future with new BufferedImage
      * */
    def correlate(kernel: Array[Array[Float]], parallelism: Int): Future[BufferedImage] = {
      new Correlation(image, kernel, parallelism).compute()
    }

    /**
      * Applies Image Normalization algorithm
      * (https://en.wikipedia.org/wiki/Normalization_(image_processing)
      * @param min minimum channel value
      * @param max maximum channel value
      * @return new BufferedImage with modified pixels
      * */
    def normalize(min: Float, max: Float): BufferedImage = {
      val (minValue, maxValue) = findMinMaxChannelValue()
      if (min != minValue || max != maxValue) {
        withNewFlattenPixels(flattenPixels.map { p =>
          (p - minValue) / (maxValue - minValue) * (max - min) + min
        })
      } else {
        image
      }
    }

    def resize(width: Int, height: Int): BufferedImage = {
      val temp = image.getScaledInstance(width, height, Image.SCALE_SMOOTH)
      val res = new BufferedImage(width, height, image.getType)

      val g2d = res.createGraphics()
      g2d.drawImage(temp, 0, 0, null)
      g2d.dispose()

      res
    }

    /**
      * Applies Gaussian blur filter to image
      * Computes blur radius by image resolution
      * Radius computation were chosen in such a way as to approximate CImg blur with sigma 1
      * @return new BufferedImage (blurred)
      * */
    def makeBlurred(): BufferedImage = {
      scrImage
        .filter(GaussianBlurFilter(Math.max(image.getHeight(), image.getWidth()) / 200))
        .toNewBufferedImage(image.getType)
    }

    /**
      * Applies Images Histogram Equalization algorithm
      * (https://en.wikipedia.org/wiki/Histogram_equalization)
      * @param levels histogram levels
      * @return new BufferedImage with modified pixels
      * */
    def equalize(levels: Int): BufferedImage = {
      val (min, max) = findMinMaxChannelValue()

      val hist = createHistogram(levels, min, max)
      hist.zipWithIndex.foldLeft(0L) { case (acc, (x, i)) =>
        hist(i) += acc
        hist(i)
      }

      val last = if (hist.isEmpty || hist.last == 0) 1 else hist.last

      val res = flattenPixels.map { p =>
        val pos = ((p - min) * (levels - 1).toFloat / (max - min)).toInt
        if (pos >= 0 && pos < levels) min + (max - min) * hist(pos).toFloat / last else p
      }

      withNewFlattenPixels(res)
    }

    /**
      * Compares images by pixels
      * */
    def isEqualTo(other: BufferedImage): Boolean = {
      if (image.getWidth() != other.getWidth() || image.getHeight() != other.getHeight()) {
        false
      } else {
        coordinates.foldLeft(true) {
          case (res, (x, y)) => res && image.getRGB(x, y) == other.getRGB(x, y)
        }
      }
    }

    /**
      * Gets Y channel value of specified pixel
      * Use only if you sure that image has YCbCr color model!
      * @param x x pixel coordinate
      * @param y y pixel coordinate
      * @return Y channel value
      * */
    def getY(x: Int, y: Int): Int = {
      (image.getRGB(x, y) >> 0) & 0xFF
    }

    /**
      * Set's Y channel value to specified pixel
      * Use only if you sure that image has YCbCr color model!
      * @param x x coordinate of pixel
      * @param y y coordinate of pixel
      * @param Y Y channel value
      * */
    def setY(x: Int, y: Int, Y: Int): Unit = {
      image.setRGB(x, y, Y << 16)
    }

    /**
      * Finds max channel value image's pixels
      * Example: image has 2 pixel in RGB: (12, 4, 1), (32, 44, 2)
      *          function with will return 44
      * @return max channel value
      * */
    def max(): Int = flattenPixels.max

    /**
      * Divides each channel value of each pixel into x
      * Example: image has 1 pixel in RGB: (12, 4, 1)
      *          function with x == 2 will produce new image with pixel (6, 2, 0)
      * @return new BufferedImage with modified pixels
      * */
    def / (x: Int): BufferedImage = withNewFlattenPixels(flattenPixels.map(_ / x.toFloat))

    /**
      * Raises each channel value of each pixel to the power of x
      * Example: image has 1 pixel in RGB: (12, 4, 1)
      *          function with x == 2 will produce new image with pixel (144, 16, 1)
      * @return new BufferedImage with modified pixels
      * */
    def pow(x: Int): BufferedImage = {
      if (x == 1) {
        image
      } else {
        withNewFlattenPixels(flattenPixels.map(p => Math.pow(p, x).toInt))
      }
    }

    def toMatrix: Array[Array[Float]] = {
      Array.tabulate(image.getWidth(), image.getHeight())((i, j) => image.getRGB(i, j))
    }

    /**
      * Creates histogram of pixel channels' values
      * Example: image has 2 pixels in RGB: (12, 4, 1) and (9, 5, 11)
      *          function with levels == 2 will return ArrayBuffer(3, 3)
      * @param levels count of histogram columns
      * @param minValue minimum value of channel for adding to histogram
      * @param maxValue maximum value of channel for adding to histogram
      * */
    private def createHistogram(levels: Int, minValue: Int, maxValue: Int): ArrayBuffer[Long] = {
      if (minValue == maxValue) { return ArrayBuffer.empty }

      val hist = ArrayBuffer.fill[Long](levels)(0)
      flattenPixels.filter(p => p >= minValue && p <= maxValue).foreach {
        case p if p == maxValue => hist(levels - 1) += 1
        case p => hist(((p - minValue) * levels.toFloat / (maxValue - minValue)).toInt) += 1
      }

      hist
    }

    /**
      * Example: image has 2 pixels in RBG: (123, 23, 124) and (12, 25, 1)
      *          function will return (1, 124)
      * @return tuple of minimum and maximum channel values
      * */
    private def findMinMaxChannelValue(): (Int, Int) = {
      flattenPixels.foldLeft(Int.MaxValue -> 0) { case ((min, max), c) =>
        Math.min(min, c) -> Math.max(max, c)
      }
    }

    private def cut(x: Int, min: Int, max: Int): Int = Math.min(Math.max(min, x), max)

    /**
      * Creates array with channel values of each pixel
      * Example: RGB image, first pixel has red value 17, green 128, blue 23
      *          function will produce Array(17, 128, 23, ...)
      * @return Array with channels' values
      * */
    private def createFlattenPixelsArray(): Array[Int] = {
      image.getRaster
        .getDataBuffer
        .asInstanceOf[DataBufferByte]
        .getData
        .grouped(image.getRaster.getNumDataElements)
        .flatMap(_.reverse) // because origin getData is not compatible with Raster.setDataElements
        .toArray
        .map {
          case x if x < 0 => x + 256 // because scala have not unsigned byte
          case x => x
        }
    }

    /**
      * Create new Buffered image with pixel values from flatten array
      * Function is generic because channel values may be Int or Float
      * @param flattenPixels Array created by createFlattenPixelsArray (may be modified)
      * @return new BufferedImage
      * */
    private def withNewFlattenPixels[T](flattenPixels: Array[T], imageType: Int = image.getType)(implicit num: Numeric[T]): BufferedImage = {
      val temp = new BufferedImage(image.getWidth(), image.getHeight(), imageType)
      temp.getRaster.setDataElements(0, 0, image.getWidth(), image.getHeight(), flattenPixels.map {
        case x if num.toFloat(x) > 127 => (num.toFloat(x) - 256).toByte
        case x => num.toFloat(x).toByte
      })
      temp
    }
  }

  /**
    * Method class for Image correlation algorithm
    * (https://refactoring.com/catalog/replaceMethodWithMethodObject.html)
    * */
  class Correlation(image: BufferedImage, kernel: Array[Array[Float]], parallelism: Int) {
    private val res = new BufferedImage(image.getWidth(), image.getHeight(), image.getType)
    private val futuresCount = if (parallelism > 1) parallelism - 1 else 1

    private val mx1, my1 = kernel.length / 2
    private val (mx2, my2) = (kernel.length - mx1 - 1, kernel.length - my1 - 1)
    private val (mxe, mye) = (image.getWidth() - mx2, image.getHeight() - my2)
    /**
      * Applies Image Correlation algorithm
      * (https://en.wikipedia.org/wiki/Digital_image_correlation)
      * @return Future with new BufferedImage
      * */
    def compute(): Future[BufferedImage] = {
      val innerProcesses = computeInnerPixels()

      val outerProcess = computeOuterPixels()

      for {
        _ <- innerProcesses
        _ <- outerProcess
      } yield res
    }

    private def computeInnerPixels(): Future[Iterator[Unit]] = {
      Future.traverse((my1 until mye).grouped(futuresCount)) { ys =>
        Future {
          for (y <- ys; x <- mx1 until mxe) {
            val value = (for {
              ym <- -my1 to my2
              xm <- -mx1 to mx2
            } yield image.getY(x + xm, y + ym) * kernel(mx1 + xm)(my1 + ym)).sum

            res.setY(x, y, Math.round(value))
          }
        }
      }
    }

    private def computeOuterPixels(): Future[Unit] = {
      Future {
        for (y <- 0 until image.getHeight()) {
          var x = 0
          while (x < image.getWidth()) {
            val value = (for (ym <- -my1 to my2; xm <- -mx1 to mx2) yield {
              if (x + xm < 0
                || x + xm >= image.getWidth()
                || y + ym < 0
                || y + ym >= image.getHeight()) {
                0
              } else {
                image.getY(x + xm,y + ym) * kernel(mx1 + xm)(my1 + ym)
              }
            }).sum

            res.setY(x, y, Math.round(value))

            if (!(y < my1 || y >= mye) && !(x < mx1 - 1 || x >= mxe)) {
              x = mxe
            } else {
              x += 1
            }
          }
        }
      }
    }

  }

}
