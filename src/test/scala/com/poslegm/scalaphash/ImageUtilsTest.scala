package com.poslegm.scalaphash

import java.io.File
import java.util.Calendar
import javax.imageio.ImageIO

import com.poslegm.scalaphash.ImageUtils._
import org.scalatest.{AsyncFlatSpec, Matchers, PrivateMethodTester}

class ImageUtilsTest extends AsyncFlatSpec with Matchers with PrivateMethodTester {
  "BufferedImage equal check" should "work correct" in {
    val a = ImageIO.read(new File("src/test/resources/example1.jpg"))
    val b = ImageIO.read(new File("src/test/resources/example1.jpg"))

    a.isEqualTo(b) should be (true)
  }

  "MakeGrayscale" should "work correct" in {
    val a = ImageIO.read(new File("src/test/resources/example1.jpg")).makeGrayScale()
    val b = ImageIO.read(new File("src/test/resources/example1.jpg")).makeGrayScale()

    ImageIO.write(a, "jpg", new File("src/test/resources/grayscaled-example1.jpg"))
    a.isEqualTo(b) should be (true)
  }

  "MakeConvolved" should "work correct" in {
    for {
      a <- ImageIO.read(new File("src/test/resources/example1.jpg")).makeGrayScale().makeConvolved(4)
      b <- ImageIO.read(new File("src/test/resources/example1.jpg")).makeGrayScale().makeConvolved(4)
    } yield {
      ImageIO.write(a, "jpg", new File("src/test/resources/convolved-example1.jpg"))
      a.isEqualTo(b) should be(true)
    }
  }

  "MakeBlured" should "work correct" in {
    val a = ImageIO.read(new File("src/test/resources/example1.jpg")).makeBlurred()
    val b = ImageIO.read(new File("src/test/resources/example1.jpg")).makeBlurred()

    ImageIO.write(a, "jpg", new File("src/test/resources/blured-example1.jpg"))

    a.isEqualTo(b) should be (true)
  }

  "Resize" should "work correct" in {
    val a = ImageIO.read(new File("src/test/resources/example1.jpg"))
    val b = ImageIO.read(new File("src/test/resources/example1.jpg"))

    ImageIO.write(a.resize(32, 32), "jpg", new File("src/test/resources/resized-example1.jpg"))

    a.resize(32, 32).isEqualTo(b.resize(32, 32)) should be (true)
  }

  "Equalize" should "work correct" in {
    val a = ImageIO.read(new File("src/test/resources/example4.jpg"))
    val b = ImageIO.read(new File("src/test/resources/example4.jpg"))

    ImageIO.write(a.equalize(256), "jpg", new File("src/test/resources/equalized-example4.jpg"))

    a.equalize(256).isEqualTo(b.equalize(256)) should be (true)
  }
}
