package com.poslegm.scalaphash

import java.awt.image._
import java.awt.{Color, Image}

import com.sksamuel.scrimage
import com.sksamuel.scrimage.filter.GaussianBlurFilter

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object ImageUtils {

  // TODO DON'T USE getRGB!!!
  // TODO decompose methods
  // TODO optimize scrimage
  // TODO use componentSize for grayscale checks
  implicit class BufferedImageExtended(image: BufferedImage) {
    private lazy val scrImage = scrimage.Image.fromAwt(image)
    private lazy val flattenPixels = createFlattenPixelsArray()
    private lazy val coordinates = for {
      x <- 0 until image.getWidth()
      y <- 0 until image.getHeight()
    } yield (x, y)

    /**
      * Makes image grayscale by converting RGB to YCbCr and keeping Y channel only
      * */
    def makeGrayScale(): BufferedImage = {
      if (image.getType == BufferedImage.TYPE_BYTE_GRAY || image.getType == BufferedImage.TYPE_USHORT_GRAY) {
        return image
      }

      val grayScaledImage = new BufferedImage(
        image.getWidth,
        image.getHeight,
        BufferedImage.TYPE_BYTE_GRAY
      )

      val temp = new BufferedImage(image.getWidth, image.getHeight, BufferedImage.TYPE_INT_RGB)

      val coordinates = for {
        x <- 0 until image.getWidth
        y <- 0 until image.getHeight
      } yield (x, y)

      coordinates.foreach {
        case (x, y) =>
          val pixel = new Color(image.getRGB(x, y))
          val (r, g, b) = (pixel.getRed, pixel.getGreen, pixel.getBlue)

          val Y = cut(((66 * r + 129 * g + 25 * b + 128) / 256.0f + 16).toInt, 0, 255)
          val newPixel = (Y << 16) | (Y << 8) | Y

          temp.setRGB(x, y, newPixel)
      }

      val g = grayScaledImage.getGraphics
      g.drawImage(temp, 0, 0, null)
      g.dispose()

      grayScaledImage
    }

    // TODO parallelism
    def makeConvolved(kernelSize: Int = 7): Future[BufferedImage] = {
      val kernel = Array.tabulate(kernelSize, kernelSize)((_, _) => 1.0f / (kernelSize * kernelSize))

      correlate(kernel)
    }

    def correlate[T](kernel: Array[Array[Float]]): Future[BufferedImage] = {
      val res = new BufferedImage(image.getWidth(), image.getHeight(), image.getType)

      val mx1, my1 = kernel.length / 2
      val (mx2, my2) = (kernel.length - mx1 - 1, kernel.length - my1 - 1)
      val (mxe, mye) = (image.getWidth() - mx2, image.getHeight() - my2)

      val innerProcesses = Future.traverse((my1 until mye).grouped(3)) { ys =>
        Future {
          for {
            y <- ys
            x <- mx1 until mxe
          } {
            val value = (for {
              ym <- -my1 to my2
              xm <- -mx1 to mx2
            } yield getY(x + xm, y + ym) * kernel(mx1 + xm)(my1 + ym)).sum

            setY(res, x, y, Math.round(value))
          }
        }
      }

      val outerProcess = Future {
        for (y <- 0 until image.getHeight()) {
          var x = 0
          while (x < image.getWidth()) {
            val value = (for {
              ym <- -my1 to my2
              xm <- -mx1 to mx2
            } yield {
              if (x + xm < 0
                || x + xm >= image.getWidth()
                || y + ym < 0
                || y + ym >= image.getHeight()) {
                0
              } else {
                getY(x + xm,y + ym) * kernel(mx1 + xm)(my1 + ym)
              }
            }).sum

            setY(res, x, y, Math.round(value))

            if (!(y < my1 || y >= mye) && !(x < mx1 - 1 || x >= mxe)) {
              x = mxe
            } else {
              x += 1
            }
          }
        }
      }

      for {
        _ <- innerProcesses
        _ <- outerProcess
      } yield res
    }

    def normalize(min: Float, max: Float): BufferedImage = {
      val (minValue, maxValue) = findMinMaxChannelValue()
      if (min != minValue || max != maxValue) {
        withNewFlattenPixels(flattenPixels.map { p =>
          ((p - minValue) / (maxValue - minValue) * (max - min) + min).toInt
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

    // TODO benchmark and test marr
    def makeBlurred(sigma: Int): BufferedImage = {
      scrImage.filter(GaussianBlurFilter(Math.max(image.getHeight(), image.getWidth()) / 200)).toNewBufferedImage(image.getType)
    }

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
        if (pos >= 0 && pos < levels) { min + (max - min) * hist(pos).toFloat / last } else { p }
      }

      val temp = new BufferedImage(image.getWidth(), image.getHeight(), image.getType)

      // TODO create different method
      temp.getRaster.setDataElements(0, 0, image.getWidth(), image.getHeight(), res.map {
        case x if x > 127 => (x - 256).toByte
        case x => x.toByte
      })
      temp
    }

    private def createHistogram(levels: Int, minValue: Int, maxValue: Int): ArrayBuffer[Long] = {
      if (minValue == maxValue) { return ArrayBuffer.empty }

      val hist = ArrayBuffer.fill[Long](levels)(0)
      flattenPixels.filter(p => p >= minValue && p <= maxValue).foreach {
        case p if p == maxValue => hist(levels - 1) += 1
        case p => hist(((p - minValue) * levels.toFloat / (maxValue - minValue)).toInt) += 1
      }

      hist
    }

    private def findMinMaxChannelValue(): (Int, Int) = {
      flattenPixels.foldLeft(Int.MaxValue -> 0) { case ((min, max), c) =>
        Math.min(min, c) -> Math.max(max, c)
      }
    }

    private def cut(x: Int, min: Int, max: Int): Int = Math.min(Math.max(min, x), max)

    def isEqualTo(other: BufferedImage): Boolean = {
      if (image.getWidth() != other.getWidth() || image.getHeight() != other.getHeight()) {
        false
      } else {
        val coordinates = for {
          x <- 0 until image.getWidth
          y <- 0 until image.getHeight
        } yield (x, y)

        coordinates.foldLeft(true) {
          case (res, (x, y)) => res && image.getRGB(x, y) == other.getRGB(x, y)
        }
      }
    }

    def getY(x: Int, y: Int): Int = {
      (image.getRGB(x, y) >> 0) & 0xFF
    }

    private def setY(target: BufferedImage, x: Int, y: Int, Y: Int): Unit = {
      target.setRGB(x, y, Y << 16)
    }

    def max(): Int = flattenPixels.max

    def / (x: Int): BufferedImage = withNewFlattenPixels(flattenPixels.map(_ / x))

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

    private def withNewFlattenPixels(flattenPixels: Array[Int]): BufferedImage = {
      val temp = new BufferedImage(image.getWidth(), image.getHeight(), image.getType)
      temp.getRaster.setDataElements(0, 0, image.getWidth(), image.getHeight(), flattenPixels.map {
        case x if x > 127 => (x - 256).toByte
        case x => x.toByte
      })
      temp
    }
  }

}
