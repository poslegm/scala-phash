package com.github.poslegm.scalaphash

import java.io.File
import javax.imageio.ImageIO

import org.scalatest.{FlatSpec, Matchers, PrivateMethodTester}

class PHashTest extends FlatSpec with Matchers with PrivateMethodTester {
  "PHash" should "create correct dct matrix" in {
    val canonical = Array(
      Array(0.408248, 0.557678, 0.5, 0.408248, 0.288675, 0.149429),
      Array(0.408248, 0.408248, 3.53525e-17, -0.408248, -0.57735, -0.408248),
      Array(0.408248, 0.149429, -0.5, -0.408248, 0.288675, 0.557678),
      Array(0.408248, -0.149429, -0.5, 0.408248, 0.288675, -0.557678),
      Array(0.408248, -0.408248, -1.06058e-16, 0.408248, -0.57735, 0.408248),
      Array(0.408248, -0.557678, 0.5, -0.408248, 0.288675, -0.149429)
    ).map(_.map(_.toFloat))

    val createDctMatrix = PrivateMethod[Array[Array[Float]]]('createDctMatrix)
    val matrix = PHash invokePrivate createDctMatrix(6)
    matrix.length shouldEqual canonical.length
    matrix(0).length shouldEqual canonical(0).length

    (for {
      i <- matrix.indices
      j <- matrix(i).indices
    } yield approximatelyEqual(matrix(i)(j), canonical(i)(j), 0.000001f))
      .forall(x => x) should be(true)
  }

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
    } yield approximatelyEqual(matrix(i)(j), canonical(i)(j), 0.000001f))
      .forall(x => x) should be(true)
  }

  "PHash" should "compute dct hashes" in {
    val example2 = ImageIO.read(new File("src/test/resources/example2.jpg"))

    val example2DctHash = PHash.dctHash(example2)
    example2DctHash shouldEqual Right(1169849770)
  }

  "DCT hash" should "compare not equal" in {
    val example2 = ImageIO.read(new File("src/test/resources/example2.jpg"))
    val example3 = ImageIO.read(new File("src/test/resources/example3.jpg"))

    (for {
      example2DctHash <- PHash.dctHash(example2)
      example3DctHash <- PHash.dctHash(example3)
    } yield PHash.dctHashDistance(example2DctHash, example3DctHash)) shouldEqual Right(37)
  }

  "DCT hash" should "compare dog and cat" in {
    val dog = ImageIO.read(new File("src/test/resources/1.jpg"))
    val cat = ImageIO.read(new File("src/test/resources/2.jpg"))

    (for {
      dogDctHash <- PHash.dctHash(dog)
      catDctHash <- PHash.dctHash(cat)
    } yield PHash.dctHashDistance(dogDctHash, catDctHash)) shouldEqual Right(36)
  }

  "DCT hash" should "compare equal" in {
    val example2 = ImageIO.read(new File("src/test/resources/example2.jpg"))
    val example4 = ImageIO.read(new File("src/test/resources/example4.jpg"))

    (for {
      example2DctHash <- PHash.dctHash(example2)
      example4DctHash <- PHash.dctHash(example4)
    } yield PHash.dctHashDistance(example2DctHash, example4DctHash)) shouldEqual Right(1)
  }

  "PHash" should "compute marr hashes" in {
    val example2 = ImageIO.read(new File("src/test/resources/example2.jpg"))

    val example2MarrHash = PHash.marrHash(example2)
    example2MarrHash.right.get shouldEqual Array(0, 0, 0, 9, 108, 152, 0, 0, 0, 0, 0, 0, 96, 0, 9, 96, 0, 0, 13, 182,
      208, 132, 178, 92, 13, 51, 217, 166, 175, 9, 172, 5, 130, 48, 109, 76, 53, 100, 164, 154, 129, 221, 172, 152, 183,
      52, 130, 15, 1, 243, 190, 143, 240, 73, 106, 134, 78, 192, 245, 193, 11, 64, 122, 19, 165, 177, 177, 132, 144,
      212, 104, 102)
  }

  "Marr hashes" should "compare not equal" in {
    val example2 = ImageIO.read(new File("src/test/resources/example2.jpg"))
    val example3 = ImageIO.read(new File("src/test/resources/example3.jpg"))

    (for {
      example2MarrHash <- PHash.marrHash(example2)
      example3MarrHash <- PHash.marrHash(example3)
    } yield PHash.marrHashDistance(example2MarrHash, example3MarrHash)) shouldEqual Right(Some(0.3697916666666667))
  }

  "Marr hashes" should "compare equal" in {
    val example2 = ImageIO.read(new File("src/test/resources/example2.jpg"))
    val example4 = ImageIO.read(new File("src/test/resources/example4.jpg"))

    (for {
      example2MarrHash <- PHash.marrHash(example2)
      example4MarrHash <- PHash.marrHash(example4)
    } yield PHash.marrHashDistance(example2MarrHash, example4MarrHash)) shouldEqual Right(Some(0.3315972222222222))
  }

  "Marr hashes" should "compare dog and cat" in {
    val dog = ImageIO.read(new File("src/test/resources/1.jpg"))
    val cat = ImageIO.read(new File("src/test/resources/2.jpg"))

    (for {
      dogMarrHash <- PHash.marrHash(dog)
      catMarrHash <- PHash.marrHash(cat)
    } yield PHash.marrHashDistance(dogMarrHash, catMarrHash)) shouldEqual Right(Some(0.4947916666666667))
  }

  "PHash" should "compute radial hashes" in {
    val example2 = ImageIO.read(new File("src/test/resources/example2.jpg"))

    val example2RadialHash = PHash.radialHash(example2)

    example2RadialHash.right.get shouldEqual Array(194, 192, 0, 204, 89, 209, 193, 163, 255, 212, 187, 207, 185, 190,
      175, 203, 179, 182, 189, 200, 186, 191, 194, 196, 194, 201, 199, 189, 187, 197, 193, 192, 198, 191, 194, 194, 197,
      195, 192, 196)
  }

  "Radial hash" should "compare not equal" in {
    val example2 = ImageIO.read(new File("src/test/resources/example2.jpg"))
    val example3 = ImageIO.read(new File("src/test/resources/example3.jpg"))

    (for {
      example2RadialHash <- PHash.radialHash(example2)
      example3RadialHash <- PHash.radialHash(example3)
    } yield PHash.radialHashDistance(example2RadialHash, example3RadialHash)) shouldEqual Right(0.8628696879236165)
  }

  "Radial hash" should "compare equal" in {
    val example2 = ImageIO.read(new File("src/test/resources/example2.jpg"))
    val example4 = ImageIO.read(new File("src/test/resources/example4.jpg"))

    (for {
      example2RadialHash <- PHash.radialHash(example2)
      example4RadialHash <- PHash.radialHash(example4)
    } yield PHash.radialHashDistance(example2RadialHash, example4RadialHash)) shouldEqual Right(0.9538751316650709)
  }

  "Radial hash" should "compare dog and cat" in {
    val dog = ImageIO.read(new File("src/test/resources/1.jpg"))
    val cat = ImageIO.read(new File("src/test/resources/2.jpg"))

    (for {
      dogRadialHash <- PHash.radialHash(dog)
      catRadialHash <- PHash.radialHash(cat)
    } yield PHash.radialHashDistance(dogRadialHash, catRadialHash)) shouldEqual Right(0.36438994709451805)
  }

  def approximatelyEqual(x: Float, y: Float, delta: Float): Boolean =
    y - delta <= x && x <= y + delta
}
