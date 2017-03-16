import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO
import ImageUtils._

object PHash {
  def dctHash(image: BufferedImage): Long = {
    image.sp
  }

  def createDctMatrix(size: Int): Array[Array[Float]] = {
    val c = Math.sqrt(2.0 / size).toFloat
    Array.tabulate(size, size) {
      case (_, 0) => 1 / Math.sqrt(size).toFloat
      case (x, y) => c * Math.cos((Math.PI / 2 / size) * y * (2 * x + 1)).toFloat
    }
  }
}
