package com.github.poslegm.scalaphash

import java.awt.color.ColorSpace
import java.awt.image._
import java.awt.{Image, Transparency}

import com.jhlabs.image.GaussianFilter

import scala.collection.mutable.ArrayBuffer

private[scalaphash] case class PixelMatrix private (private val flattenPixels: Array[Float],
                                                    private val pixelElementsCount: Int,
                                                    width: Int,
                                                    height: Int,
                                                    private val imageType: Int) {
  /**
    * Makes image grayscale by converting RGB to YCbCr and keeping Y channel only
    *
    * @return PixelMatrix with Y channel only
    * */
  def makeGrayScale(): PixelMatrix = {
    if (pixelElementsCount < 3) {
      this.copy(imageType = BufferedImage.TYPE_BYTE_GRAY)
    } else {
      val grayPixels = (flattenPixels.indices by pixelElementsCount).map { i =>
        val (r, g, b) = (flattenPixels(i + 2), flattenPixels(i + 1), flattenPixels(i))
        cut((66 * r + 129 * g + 25 * b + 128) / 256.0f + 16, 0, 255)
      }.toArray

      this.copy(
        imageType = BufferedImage.TYPE_BYTE_GRAY,
        pixelElementsCount = 1,
        flattenPixels = grayPixels
      )
    }
  }

  /**
    * Applies Image Convolution algorithm
    * (https://en.wikipedia.org/wiki/Kernel_(image_processing)#Convolution)
    * @param kernelSize size of matrix
    * @return new PixelMatrix with modified pixels
    * */
  def makeConvolved(kernelSize: Int = 7): PixelMatrix = {
    val kernel = Array.tabulate(kernelSize, kernelSize)((_, _) => 1.0f / (kernelSize * kernelSize))

    correlate(kernel)
  }

  /**
    * Applies Image Correlation algorithm
    * (https://en.wikipedia.org/wiki/Digital_image_correlation)
    * @param kernel correlation matrix
    * @return new PixelMatrix with modified pixels
    * */
  def correlate(kernel: Array[Array[Float]]): PixelMatrix = {
    new Correlation(this, kernel).compute()
  }

  /**
    * Applies Image Normalization algorithm
    * (https://en.wikipedia.org/wiki/Normalization_(image_processing)
    * @param min minimum channel value
    * @param max maximum channel value
    * @return new PixelMatrix with modified pixels
    * */
  def normalize(min: Float, max: Float): PixelMatrix = {
    val (minValue, maxValue) = findMinMaxChannelValue()
    if (min != minValue || max != maxValue) {
      val normalizedPixels = flattenPixels.map { p =>
        (p - minValue) / (maxValue - minValue) * (max - min) + min
      }
      this.copy(flattenPixels = normalizedPixels)
    } else {
      this
    }
  }

  /**
    * Applies resize algorithm (scale smooth)
    * @param dstWidth width of new image
    * @param dstHeight height of new image
    * @return resized PixelMatrix
    * */
  def resize(dstWidth: Int, dstHeight: Int): PixelMatrix = {
    val temp = toBufferedImage().getScaledInstance(dstWidth, dstHeight, Image.SCALE_SMOOTH)
    val res = new BufferedImage(dstWidth, dstHeight, imageType)

    val g2d = res.createGraphics()
    g2d.drawImage(temp, 0, 0, null)
    g2d.dispose()

    PixelMatrix(res)
  }

  /**
    * Applies Gaussian blur filter to image
    * Computes blur radius by image resolution
    * Radius computation were chosen in such a way as to approximate CImg blur with sigma 1
    * @return new PixelMatrix (blurred)
    * */
  def makeBlurred(): PixelMatrix = {
    val dst = new BufferedImage(width, height, imageType)
    new GaussianFilter(Math.max(height, width) / 200).filter(toBufferedImage(), dst)
    PixelMatrix(dst)
  }

  /**
    * Applies Images Histogram Equalization algorithm
    * (https://en.wikipedia.org/wiki/Histogram_equalization)
    * @param levels histogram levels
    * @return new PixelMatrix with modified pixels
    * */
  def equalize(levels: Int): PixelMatrix = {
    val (min, max) = findMinMaxChannelValue()

    val hist = createHistogram(levels, min, max)
    hist.zipWithIndex.foldLeft(0L) { case (acc, (_, i)) =>
      hist(i) += acc
      hist(i)
    }

    val last = if (hist.isEmpty || hist.last == 0) 1 else hist.last

    val res = flattenPixels.map { p =>
      val pos = ((p - min) * (levels - 1).toFloat / (max - min)).toByte
      if (pos >= 0 && pos < levels) min + (max - min) * hist(pos).toFloat / last else p
    }

    this.copy(flattenPixels = res)
  }

  /**
    * Compares images by each pixel
    * */
  def isEqualTo(other: PixelMatrix): Boolean = {
    if (width != other.width || height != other.height) {
      false
    } else {
      flattenPixels.sameElements(other.flattenPixels)
    }
  }

  /**
    * Gets Y channel value of specified pixel
    * Use only if you sure that image has YCbCr color model!
    * @param x x pixel coordinate
    * @param y y pixel coordinate
    * @return Y channel value
    * */
  def getY(x: Int, y: Int): Int = processedColors(y * width + x) & 0xFF

  /**
    * Gets Y channel value of specified pixels
    * Use only if you sure that image has YCbCr color model!
    * @param x x pixel coordinate
    * @param y y pixel coordinate
    * @param w width of region
    * @param h height of region
    * @return Y channel value
    * */
  def getY(x: Int, y: Int, w: Int, h: Int): Array[Int] = {
    (for {
      j <- 0 until h
      i <- 0 until w
    } yield getY(x + i, y + j)).toArray
  }

  /**
    * Creates 2-dimension matrix with Y channel values of each pixel
    * Use only if you sure that image has YCbCr color model!
    * @return 2-dimension Array with image pixels
    * */
  def toMatrix: Array[Array[Float]] = {
    Array.tabulate(width, height)((i, j) => processedColors(j * width + i))
  }

  /**
    * Create new Buffered image with pixel values from flatten array
    * @param imageType type of image
    * @return new BufferedImage
    * */
  def toBufferedImage(imageType: Int = imageType): BufferedImage = {
    val temp = new BufferedImage(width, height, imageType)
    imageType match {
      case t if t < 5 => temp.getRaster.setDataElements(0, 0, width, height, flattenPixels.map(_.toInt))
      case 8 | 9 | 11 => temp.getRaster.setDataElements(0, 0, width, height, flattenPixels.map(_.toShort))
      case _ => temp.getRaster.setDataElements(0, 0, width, height, flattenPixels.map(_.toByte))
    }
    temp
  }


  /**
    * Y channel values of flatten pixels after color processing
    * Use only if you sure that image has YCbCr color model!
    * */
  private lazy val processedColors = grayBufferedImage.getRGB(0, 0, width, height, null, 0, width)

  private lazy val grayBufferedImage = {
    val gbi = new BufferedImage(width, height, BufferedImage.TYPE_BYTE_GRAY)
    gbi.getRaster.setDataElements(0, 0, width, height, flattenPixels.map(_.toByte))
    gbi
  }

  /**
    * Creates histogram of pixel channels' values
    * Example: image has 2 pixels in RGB: (12, 4, 1) and (9, 5, 11)
    *          function with levels == 2 will return ArrayBuffer(3, 3)
    * @param levels count of histogram columns
    * @param minValue minimum value of channel for adding to histogram
    * @param maxValue maximum value of channel for adding to histogram
    * */
  private def createHistogram(levels: Int, minValue: Float, maxValue: Float): ArrayBuffer[Long] = {
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
    flattenPixels.foldLeft(Int.MaxValue -> Int.MinValue) { case ((min, max), c) =>
      Math.min(min, c.toInt) -> Math.max(max, c.toInt)
    }
  }

  private def cut(x: Float, min: Int, max: Int): Float = Math.min(Math.max(min, x), max)

  /**
    * Method class for Image correlation algorithm
    * Works only with YCbCr!
    * (https://refactoring.com/catalog/replaceMethodWithMethodObject.html)
    * */
  private[scalaphash] class Correlation(image: PixelMatrix, kernel: Array[Array[Float]]) {
    private val res = Array.fill(image.width * image.height * image.pixelElementsCount)(0f)

    private val mx1, my1 = kernel.length / 2
    private val (mx2, my2) = (kernel.length - mx1 - 1, kernel.length - my1 - 1)
    private val (mxe, mye) = (image.width - mx2, image.height - my2)
    /**
      * Applies Image Correlation algorithm
      * (https://en.wikipedia.org/wiki/Digital_image_correlation)
      * @return new PixelMatrix
      * */
    def compute(): PixelMatrix = {
      computeInnerPixels()
      computeOuterPixels()
      image.copy(flattenPixels = res)
    }

    private def computeInnerPixels(): Unit = {
      for (y <- my1 until mye; x <- mx1 until mxe) {
        var value = 0f
        for (ym <- -my1 to my2; xm <- -mx1 to mx2) {
          value += image.getY(x + xm, y + ym) * kernel(mx1 + xm)(my1 + ym)
        }
        res(y * image.width + x) = PixelMatrix.getYValue(Math.round(value))
      }
    }

    private def computeOuterPixels(): Unit = {
      for (y <- 0 until image.height) {
        var x = 0
        while (x < image.width) {
          var value = 0f
          for (ym <- -my1 to my2; xm <- -mx1 to mx2) {
            if (!(x + xm < 0 || x + xm >= image.width || y + ym < 0 || y + ym >= image.height)) {
              value += image.getY(x + xm, y + ym) * kernel(mx1 + xm)(my1 + ym)
            }
          }

          res(y * image.width + x) = PixelMatrix.getYValue(Math.round(value))

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

private[scalaphash] object PixelMatrix {
  def apply(image: BufferedImage): PixelMatrix = {
    new PixelMatrix(
      flattenPixels = createFlattenPixelsArray(image),
      pixelElementsCount = image.getRaster.getNumDataElements,
      width = image.getWidth,
      height = image.getHeight,
      imageType = image.getType
    )
  }

  private lazy val cs = ColorSpace.getInstance(ColorSpace.CS_GRAY)
  private lazy val nBits = Array(8)
  private lazy val grayColorModel = new ComponentColorModel(
    cs, nBits, false, true, Transparency.OPAQUE, DataBuffer.TYPE_BYTE
  )

  /**
    * Compute processed color value for Y channel
    * @param Y Y channel value
    * @return Y channel value after color processing
    * */
  def getYValue(Y: Int): Float = {
    (grayColorModel.getDataElements(Y << 16, null).asInstanceOf[Array[Byte]].head & 0xFF).toFloat
  }

  /**
    * Creates array with channel values of each pixel
    * Example: RGB image, first pixel has red value 17, green 128, blue 23
    *          function will produce Array(17, 128, 23, ...)
    * @return Array with channels' values
    * */
  private def createFlattenPixelsArray(image: BufferedImage): Array[Float] = {
    val pixelElementCount = image.getColorModel.getNumComponents
    if (image.getColorModel.getComponentSize(0) == 8) {
      val buffer = Array.ofDim[Byte](image.getWidth() * image.getHeight() * pixelElementCount)
      image.getRaster.getDataElements(0, 0, image.getWidth(), image.getHeight(), buffer)
      buffer.map(p => (p & 0xFF).toFloat)
    } else {
      throw new IllegalArgumentException("Can't work with non-byte image buffers")
    }
  }
}