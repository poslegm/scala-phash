package com.poslegm.scalaphash

import java.io.File
import javax.imageio.ImageIO

import org.scalatest.{AsyncFlatSpec, Matchers, PrivateMethodTester}

class PHashTest extends AsyncFlatSpec with Matchers with PrivateMethodTester {
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
      .forall(x => x) should be (true)
  }

  "PHash" should "compute marr kernel" in {
    val canonical = Array(
      Array(-3.37606e-06, -8.5713e-05, -0.000817199, -0.00305203, -0.00469648, -0.00305203, -0.000817199, -8.5713e-05,  -3.37606e-06),
      Array(-8.5713e-05,  -0.00197456, -0.0165378,   -0.0539036,  -0.077763,   -0.0539036,  -0.0165378,   -0.00197456,  -8.5713e-05),
      Array(-0.000817199, -0.0165378,  -0.109894,    -0.246255,   -0.270671,   -0.246255,   -0.109894,    -0.0165378,   -0.000817199),
      Array(-0.00305203,  -0.0539036,  -0.246255,     0,           0.606531,    0,          -0.246255,    -0.0539036,   -0.00305203),
      Array(-0.00469648,  -0.077763,   -0.270671,     0.606531,    2,           0.606531,   -0.270671,    -0.077763,    -0.00469648),
      Array(-0.00305203,  -0.0539036,  -0.246255,     0,           0.606531,    0,          -0.246255,    -0.0539036,   -0.00305203),
      Array(-0.000817199, -0.0165378,  -0.109894,    -0.246255,   -0.270671,   -0.246255,   -0.109894,    -0.0165378,   -0.000817199),
      Array(-8.5713e-05,  -0.00197456, -0.0165378,   -0.0539036,  -0.077763,   -0.0539036,  -0.0165378,   -0.00197456,  -8.5713e-05),
      Array(-3.37606e-06, -8.5713e-05, -0.000817199, -0.00305203, -0.00469648, -0.00305203, -0.000817199, -8.5713e-05,  -3.37606e-06)
    ).map(_.map(_.toFloat))

    val createMarrKernel = PrivateMethod[Array[Array[Float]]]('createMarrKernel)
    val matrix = PHash invokePrivate createMarrKernel(1, 1)
    matrix.length shouldEqual canonical.length
    matrix.indices.foreach(i => matrix(i).length shouldEqual canonical(i).length)

    (for {
      i <- matrix.indices
      j <- matrix(i).indices
    } yield approximatelyEqual(matrix(i)(j), canonical(i)(j), 0.000001f))
      .forall(x => x) should be (true)
  }

  "PHash" should "compute dct hashes" in {
    val example2 = ImageIO.read(new File("src/test/resources/example2.jpg"))

    for {
      example2DctHash <- PHash.dctHash(example2, 4)
    } yield {
      example2DctHash shouldEqual 1169849770
    }
  }

  "DCT hash" should "compare not equal" in {
    val example2 = ImageIO.read(new File("src/test/resources/example2.jpg"))
    val example3 = ImageIO.read(new File("src/test/resources/example3.jpg"))

    for {
      example2DctHash <- PHash.dctHash(example2, 4)
      example3DctHash <- PHash.dctHash(example3, 4)
    } yield {
      PHash.dctHashDistance(example2DctHash, example3DctHash) shouldEqual 37
    }
  }

  "DCT hash" should "compare dog and cat" in {
    val dog = ImageIO.read(new File("src/test/resources/1.jpg"))
    val cat = ImageIO.read(new File("src/test/resources/2.jpg"))

    for {
      dogDctHash <- PHash.dctHash(dog, 4)
      catDctHash <- PHash.dctHash(cat, 4)
    } yield {
      PHash.dctHashDistance(dogDctHash, catDctHash) shouldEqual 36
    }
  }

  "DCT hash" should "compare equal" in {
    val example2 = ImageIO.read(new File("src/test/resources/example2.jpg"))
    val example4 = ImageIO.read(new File("src/test/resources/example4.jpg"))

    for {
      example2DctHash <- PHash.dctHash(example2, 4)
      example4DctHash <- PHash.dctHash(example4, 4)
    } yield {
      PHash.dctHashDistance(example2DctHash, example4DctHash) shouldEqual 1
    }
  }

  "PHash" should "compute marr hashes" in {
    val example2 = ImageIO.read(new File("src/test/resources/example2.jpg"))

    for {
      example2MarrHash <- PHash.marrHash(example2, 4)
    } yield {
      example2MarrHash shouldEqual Array(0, 0, 0, 42, 68, 200, 0, 0, 0, 0, 0, 8, 96, 0, 27, 32, 0, 0, 5, 166, 0, 129, 54, 194, 21, 151, 208, 48, 12, 72, 45, 150, 146, 104, 108, 225, 141, 100, 148, 147, 164, 201, 96, 144, 227, 67, 134, 76, 96, 244, 166, 79, 209, 13, 35, 140, 207, 192, 242, 158, 97, 192, 89, 131, 4, 212, 241, 116, 220, 21, 76, 92)
    }
  }

  "Marr hashes" should "compare not equal" in {
    val example2 = ImageIO.read(new File("src/test/resources/example2.jpg"))
    val example3 = ImageIO.read(new File("src/test/resources/example3.jpg"))

    for {
      example2MarrHash <- PHash.marrHash(example2, 4)
      example3MarrHash <- PHash.marrHash(example3, 4)
    } yield {
      PHash.marrHashDistance(example2MarrHash, example3MarrHash) shouldEqual Some(0.3732638888888889)
    }
  }

  "Marr hashes" should "compare equal" in {
    val example2 = ImageIO.read(new File("src/test/resources/example2.jpg"))
    val example4 = ImageIO.read(new File("src/test/resources/example4.jpg"))

    for {
      example2MarrHash <- PHash.marrHash(example2, 4)
      example4MarrHash <- PHash.marrHash(example4, 4)
    } yield {
      PHash.marrHashDistance(example2MarrHash, example4MarrHash) shouldEqual Some(0.359375)
    }
  }

  "PHash" should "compute radial hashes" in {
    val example2 = ImageIO.read(new File("src/test/resources/example2.jpg"))

    val example2DctHash = PHash.radialHash(example2)

    example2DctHash shouldEqual Array(195, 192, 0, 205, 89, 209, 194, 165, 255, 212, 188, 208, 186, 190, 176, 203, 180, 182, 190, 200, 187, 191, 195, 197, 195, 202, 200, 190, 188, 198, 194, 193, 198, 191, 194, 194, 197, 195, 193, 196)
  }

  "Radial hash" should "compare not equal" in {
    val example2 = ImageIO.read(new File("src/test/resources/example2.jpg"))
    val example3 = ImageIO.read(new File("src/test/resources/example3.jpg"))

    val example2RadialHash = PHash.radialHash(example2)
    val example3RadialHash = PHash.radialHash(example3)

    PHash.radialHashDistance(example2RadialHash, example3RadialHash) shouldEqual 0.8630427572869724
  }

  "Radial hash" should "compare equal" in {
    val example2 = ImageIO.read(new File("src/test/resources/example2.jpg"))
    val example4 = ImageIO.read(new File("src/test/resources/example4.jpg"))

    val example2RadialHash = PHash.radialHash(example2)
    val example4RadialHash = PHash.radialHash(example4)

    PHash.radialHashDistance(example2RadialHash, example4RadialHash) shouldEqual 0.9544747673204416
  }

  def approximatelyEqual(x: Float, y: Float, delta: Float): Boolean = {
    y - delta <= x && x <= y + delta
  }
}
