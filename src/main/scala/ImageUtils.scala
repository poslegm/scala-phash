import java.awt.{Color, Image}
import java.awt.image.{BufferedImage, ConvolveOp, Kernel}

object ImageUtils {

  implicit class BufferedImageExtended(image: BufferedImage) {
    /**
      * Makes image grayscale by converting RGB to YCbCr and keeping Y channel only
      * */
    def makeGrayScale(): BufferedImage = {
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

    def makeConvolved(kernelSize: Int = 7): BufferedImage = {
      val n = kernelSize * kernelSize
      val kernelData = Array.tabulate(n)(_ => 1 / n.toFloat)
      val op = new ConvolveOp(new Kernel(kernelSize, kernelSize, kernelData))

      op.filter(image, null)
    }

    def resize(width: Int, height: Int): BufferedImage = {
      val temp = image.getScaledInstance(width, height, Image.SCALE_SMOOTH)
      val res = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)

      val g2d = res.createGraphics()
      g2d.drawImage(temp, 0, 0, null)
      g2d.dispose()

      res
    }

    def makeBlured(radius: Int, horizontal: Boolean = true): BufferedImage = {
      createGaussianBlurFilter(radius, horizontal).filter(image, null)
    }

    private def createGaussianBlurFilter(radius: Int, horizontal: Boolean): ConvolveOp = {
      if (radius < 1) {
        throw new IllegalArgumentException("Radius must be >= 1")
      }

      val size = radius * 2 + 1
      val sigma = radius / 3.0f
      val twoSigmaSquare = 2.0f * sigma * sigma
      val sigmaRoot = Math.sqrt(twoSigmaSquare * Math.PI).toFloat

      val data = for {
        i <- -radius to radius
        distance = i * i
      } yield Math.exp(-distance / twoSigmaSquare).toFloat / sigmaRoot

      val total = data.sum

      val decreasedData = data.map(_ / total).toArray

      val kernel = if (horizontal) {
        new Kernel(size, 1, decreasedData)
      } else {
        new Kernel(1, size, decreasedData)
      }

      new ConvolveOp(kernel, ConvolveOp.EDGE_NO_OP, null)
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

    def printPixels(other: BufferedImage): Unit = {
      for (x <- 0 until Math.min(image.getWidth, 100)) {
        val pixel1 = image.getRGB(x, 0)
        val red1 = (pixel1 >> 16) & 0x000000FF
        val green1 = (pixel1 >> 8) & 0x000000FF
        val blue1 = pixel1 & 0x000000FF

        val pixel2 = other.getRGB(x, 0)
        val red2 = (pixel2 >> 16) & 0x000000FF
        val green2 = (pixel2 >> 8) & 0x000000FF
        val blue2 = pixel2 & 0x000000FF

        println("this: ", (red1, green1, blue1), "other: ", (red2, green2, blue2))
      }
    }

    def toMatrix: Array[Array[Float]] = {
      Array.tabulate(image.getWidth(), image.getHeight())((i, j) => image.getRGB(i, j))
    }
  }

}
