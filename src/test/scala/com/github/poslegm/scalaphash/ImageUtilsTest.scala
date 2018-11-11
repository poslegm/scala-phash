package scalaphash

import java.io.File
import javax.imageio.ImageIO

import org.scalatest.{FlatSpec, Matchers, PrivateMethodTester}

class ImageUtilsTest extends FlatSpec with Matchers with PrivateMethodTester {
  "BufferedImage equal check" should "work correct" in {
    val a = PixelMatrix(ImageIO.read(new File("src/test/resources/example1.jpg")))
    val b = PixelMatrix(ImageIO.read(new File("src/test/resources/example1.jpg")))

    a.isEqualTo(b) should be(true)
  }

  "MakeGrayscale" should "work correct" in {
    val a = PixelMatrix(ImageIO.read(new File("src/test/resources/example1.jpg"))).makeGrayScale()
    val b = PixelMatrix(ImageIO.read(new File("src/test/resources/example1.jpg"))).makeGrayScale()

    ImageIO.write(a.toBufferedImage(), "jpg", new File("src/test/resources/grayscaled-example1.jpg"))
    a.isEqualTo(b) should be(true)
  }

  "MakeConvolved" should "work correct" in {
    val a = PixelMatrix(ImageIO.read(new File("src/test/resources/example1.jpg"))).makeGrayScale().makeConvolved()
    val b = PixelMatrix(ImageIO.read(new File("src/test/resources/example1.jpg"))).makeGrayScale().makeConvolved()
    ImageIO.write(a.toBufferedImage(), "jpg", new File("src/test/resources/convolved-example1.jpg"))
    a.isEqualTo(b) should be(true)
  }

  "MakeBlured" should "work correct" in {
    val a = PixelMatrix(ImageIO.read(new File("src/test/resources/example1.jpg"))).makeBlurred()
    val b = PixelMatrix(ImageIO.read(new File("src/test/resources/example1.jpg"))).makeBlurred()

    ImageIO.write(a.toBufferedImage(), "jpg", new File("src/test/resources/blured-example1.jpg"))

    a.isEqualTo(b) should be(true)
  }

  "Resize" should "work correct" in {
    val a = PixelMatrix(ImageIO.read(new File("src/test/resources/example1.jpg"))).resize(32, 32)
    val b = PixelMatrix(ImageIO.read(new File("src/test/resources/example1.jpg"))).resize(32, 32)

    ImageIO.write(a.toBufferedImage(), "jpg", new File("src/test/resources/resized-example1.jpg"))

    a.isEqualTo(b) should be(true)
  }

  "Equalize" should "work correct" in {
    val a = PixelMatrix(ImageIO.read(new File("src/test/resources/example4.jpg")))
    val b = PixelMatrix(ImageIO.read(new File("src/test/resources/example4.jpg")))

    ImageIO.write(a.equalize(256).toBufferedImage(), "jpg", new File("src/test/resources/equalized-example4.jpg"))

    a.equalize(256).isEqualTo(b.equalize(256)) should be(true)
  }
}
