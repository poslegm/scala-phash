import java.io.File
import javax.imageio.ImageIO

import ImageUtils._
import org.scalatest.{FlatSpec, Matchers}

class PHashTest extends FlatSpec with Matchers {
    "BufferedImage equal check" should "work correct" in {
    val a = ImageIO.read(new File("src/test/resources/example1.jpg"))
    val b = ImageIO.read(new File("src/test/resources/example1.jpg"))

    a.isEqualTo(b) should be (true)
  }

  "MakeGrayscale" should "work correct" in {
    val a = ImageIO.read(new File("src/test/resources/example1.jpg"))
    val b = ImageIO.read(new File("src/test/resources/example1.jpg"))

    ImageIO.write(
      a.makeGrayScale(), "jpg", new File("src/test/resources/grayscaled-example1.jpg")
    )

    a.makeGrayScale().isEqualTo(b.makeGrayScale()) should be (true)
  }

  "MakeConvolved" should "work correct" in {
    val a = ImageIO.read(new File("src/test/resources/example1.jpg")).makeGrayScale()
    val b = ImageIO.read(new File("src/test/resources/example1.jpg")).makeGrayScale()

    ImageIO.write(
      a.makeConvolved(), "jpg", new File("src/test/resources/convolved-example1.jpg")
    )

    a.makeGrayScale().printPixels(ImageIO.read(new File("src/test/resources/canonical-grayscale.jpg")))
    a.makeConvolved().printPixels(ImageIO.read(new File("src/test/resources/canonical-res1.jpg")))

    a.makeConvolved().isEqualTo(b.makeConvolved()) should be (true)
  }

  "Resize" should "work correct" in {
    val a = ImageIO.read(new File("src/test/resources/example1.jpg"))
    val b = ImageIO.read(new File("src/test/resources/example1.jpg"))

    ImageIO.write(
      a.resize(32, 32), "jpg", new File("src/test/resources/resized-example1.jpg")
    )

    a.resize(32, 32).isEqualTo(b.resize(32, 32)) should be (true)
  }

  "PHash" should "create correct dct matrix" in {
    def approximatelyEqual(x: Float, y: Float, delta: Float): Boolean = {
      y - delta <= x && x <= y + delta
    }

    val canonical = Array(
      Array(0.408248, 0.557678, 0.5, 0.408248, 0.288675, 0.149429),
      Array(0.408248, 0.408248, 3.53525e-17, -0.408248, -0.57735, -0.408248),
      Array(0.408248, 0.149429, -0.5, -0.408248, 0.288675, 0.557678),
      Array(0.408248, -0.149429, -0.5, 0.408248, 0.288675, -0.557678),
      Array(0.408248, -0.408248, -1.06058e-16, 0.408248, -0.57735, 0.408248),
      Array(0.408248, -0.557678, 0.5, -0.408248, 0.288675, -0.149429)
    ).map(_.map(_.toFloat))

    val matrix = PHash.createDctMatrix(6)

    matrix.length shouldEqual canonical.length
    matrix(0).length shouldEqual canonical(0).length

    (for {
      i <- matrix.indices
      j <- matrix(i).indices
    } yield approximatelyEqual(matrix(i)(j), canonical(i)(j), 0.000001f)).forall(x => x) should be (true)
  }

  "PHash" should "compute dct hashes" in {
    val image = ImageIO.read(new File("src/test/resources/example1.jpg"))

    val example2 = ImageIO.read(new File("src/test/resources/example2.jpg"))
    val example3 = ImageIO.read(new File("src/test/resources/example3.jpg"))
    val example4 = ImageIO.read(new File("src/test/resources/example4_1.jpg"))

    println("eq", PHash.dctHashDistance(PHash.dctHash(example2), PHash.dctHash(example4)))
    println("neq", PHash.dctHashDistance(PHash.dctHash(example2), PHash.dctHash(example3)))

    PHash.dctHash(image) shouldEqual 1975408122
  }
}
