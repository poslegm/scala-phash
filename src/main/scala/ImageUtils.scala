import java.awt.image.{BufferedImage, ConvolveOp, Kernel}
import java.awt.{Color, Image}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object ImageUtils {

  // TODO decompose methods
  implicit class BufferedImageExtended(image: BufferedImage) {
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
      val res = new BufferedImage(image.getWidth(), image.getHeight(), image.getType)

      val kernel = Array.tabulate(kernelSize, kernelSize)((_, _) => 1.0 / (kernelSize * kernelSize))

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
            } yield getY(image, x + xm, y + ym) * kernel(mx1 + xm)(my1 + ym)).sum

            setY(res, x, y, Math.round(value).toInt)
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
                getY(image, x + xm,y + ym) * kernel(mx1 + xm)(my1 + ym)
              }
            }).sum

            setY(res, x, y, Math.round(value).toInt)

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

    def resize(width: Int, height: Int): BufferedImage = {
      val temp = image.getScaledInstance(width, height, Image.SCALE_SMOOTH)
      val res = new BufferedImage(width, height, image.getType)

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

    private def getY(target: BufferedImage, x: Int, y: Int): Int = {
      (target.getRGB(x, y) >> 0) & 0xFF
    }

    private def setY(target: BufferedImage, x: Int, y: Int, Y: Int): Unit = {
      target.setRGB(x, y, Y << 16)
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
