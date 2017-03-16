import java.awt.Image
import java.awt.image.{BufferedImage, ConvolveOp, Kernel}

object ImageUtils {

  implicit class BufferedImageExtended(image: BufferedImage) {
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

      val raster = image.getRaster
      coordinates.foreach {
        case (x, y) =>
          val pixel = raster.getPixel(x, y, Array.fill(4)(0))
          val (r, g, b) = (pixel(0), pixel(1), pixel(2))

          val Y = cut(((66 * r + 129 * g + 25 * b + 128) / 256.0f + 16).toInt, 0, 255)
          val newPixel = (Y << 16) | (Y << 8) | Y

          temp.setRGB(x, y, newPixel)
      }

      val g = grayScaledImage.getGraphics
      g.drawImage(temp, 0, 0, null)
      g.dispose()

      grayScaledImage
    }

    def makeConvolved(): BufferedImage = {
      val grayScaledImage = makeGrayScale()

      val kernelData = Array.tabulate(49)(_ => 1 / 49f)
      val op = new ConvolveOp(new Kernel(7, 7, kernelData))

      op.filter(grayScaledImage, null)
    }

    def resize(width: Int = 32, height: Int = 32): BufferedImage = {
      val temp = image.getScaledInstance(width, height, Image.SCALE_SMOOTH)
      val res = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)

      val g2d = res.createGraphics()
      g2d.drawImage(temp, 0, 0, null)
      g2d.dispose()

      res
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

        println((red1, green1, blue1), (red2, green2, blue2))
      }
    }
  }

}
