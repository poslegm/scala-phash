package com.github.poslegm.scalaphash

import java.io.File
import javax.imageio.ImageIO

import org.scalatest.{FlatSpec, Matchers, PrivateMethodTester}
import MathUtils._

class MarrHashTest extends FlatSpec with Matchers with PrivateMethodTester {
  "PHash" should "compute marr kernel" in {
    val canonical = Array(
      Array(-3.37606e-06, -8.5713e-05, -0.000817199, -0.00305203, -0.00469648, -0.00305203, -0.000817199, -8.5713e-05,
        -3.37606e-06),
      Array(-8.5713e-05, -0.00197456, -0.0165378, -0.0539036, -0.077763, -0.0539036, -0.0165378, -0.00197456,
        -8.5713e-05),
      Array(-0.000817199, -0.0165378, -0.109894, -0.246255, -0.270671, -0.246255, -0.109894, -0.0165378, -0.000817199),
      Array(-0.00305203, -0.0539036, -0.246255, 0, 0.606531, 0, -0.246255, -0.0539036, -0.00305203),
      Array(-0.00469648, -0.077763, -0.270671, 0.606531, 2, 0.606531, -0.270671, -0.077763, -0.00469648),
      Array(-0.00305203, -0.0539036, -0.246255, 0, 0.606531, 0, -0.246255, -0.0539036, -0.00305203),
      Array(-0.000817199, -0.0165378, -0.109894, -0.246255, -0.270671, -0.246255, -0.109894, -0.0165378, -0.000817199),
      Array(-8.5713e-05, -0.00197456, -0.0165378, -0.0539036, -0.077763, -0.0539036, -0.0165378, -0.00197456,
        -8.5713e-05),
      Array(-3.37606e-06, -8.5713e-05, -0.000817199, -0.00305203, -0.00469648, -0.00305203, -0.000817199, -8.5713e-05,
        -3.37606e-06)
    ).map(_.map(_.toFloat))

    val createMarrKernel = PrivateMethod[Array[Array[Float]]]('createMarrKernel)
    val matrix = PHash invokePrivate createMarrKernel(1, 1)
    matrix.length shouldEqual canonical.length
    matrix.indices.foreach(i => matrix(i).length shouldEqual canonical(i).length)

    (for {
      i <- matrix.indices
      j <- matrix(i).indices
    } yield matrix(i)(j) ~= canonical(i)(j)).forall(x => x) should be(true)
  }

  "PHash" should "compute marr hashes" in {
    val image = ImageIO.read(new File("src/test/resources/lenna.jpg"))

    PHash.marrHash(image).map(_.toList) shouldEqual Right(
      List(33, 228, 155, 210, 21, 192, 126, 154, 53, 22, 221, 31, 24, 82, 240, 204, 201, 136, 225, 234, 33, 203, 27,
        236, 57, 67, 33, 34, 222, 17, 132, 97, 43, 130, 184, 54, 171, 68, 197, 226, 194, 164, 203, 3, 82, 137, 31, 125,
        3, 246, 96, 41, 7, 192, 133, 8, 136, 66, 158, 73, 17, 15, 55, 6, 36, 6, 56, 131, 52, 206, 13, 42)
    )
  }

  "Marr hashes" should "compare not equal" in {
    val bag1 = ImageIO.read(new File("src/test/resources/bag1.jpg"))
    val bag2 = ImageIO.read(new File("src/test/resources/bag2.jpg"))

    (for {
      bag1MarrHash <- PHash.marrHash(bag1)
      bag2MarrHash <- PHash.marrHash(bag2)
    } yield PHash.marrHashDistance(bag1MarrHash, bag2MarrHash)) shouldEqual Right(Some(0.4114583333333333))
  }

  "Marr hashes" should "compare equal" in {
    val origin = ImageIO.read(new File("src/test/resources/lenna.jpg"))
    val modified = ImageIO.read(new File("src/test/resources/lenna2.jpg"))

    (for {
      originMarrHash <- PHash.marrHash(origin)
      modifiedMarrHash <- PHash.marrHash(modified)
    } yield PHash.marrHashDistance(originMarrHash, modifiedMarrHash)) shouldEqual Right(Some(0.5052083333333334))
  }

  "Marr hash" should "compare nature" in {
    val a = ImageIO.read(new File("src/test/resources/mountain1.jpeg"))
    val b = ImageIO.read(new File("src/test/resources/mountain2.jpeg"))

    (for {
      aMarrHash <- PHash.marrHash(a)
      bMarrHash <- PHash.marrHash(b)
    } yield PHash.marrHashDistance(aMarrHash, bMarrHash).map(_.toFloat ~= 0.484375F)) shouldEqual Right(Some(true))
  }

  "Marr hashes" should "compare dog and cat" in {
    val dog = ImageIO.read(new File("src/test/resources/1.jpg"))
    val cat = ImageIO.read(new File("src/test/resources/2.jpg"))

    (for {
      dogMarrHash <- PHash.marrHash(dog)
      catMarrHash <- PHash.marrHash(cat)
    } yield PHash.marrHashDistance(dogMarrHash, catMarrHash)) shouldEqual Right(Some(0.4704861111111111))
  }
}
