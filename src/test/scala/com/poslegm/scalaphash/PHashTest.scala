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
      example2MarrHash shouldEqual Array(0, 0, 0, 56, 68, 200, 0, 0, 0, 0, 0, 0, 96, 0, 27, 36, 0, 0, 13, 166, 96, 141, 50, 219, 5, 50, 16, 184, 78, 17, 244, 194, 154, 106, 45, 11, 226, 228, 180, 147, 36, 208, 76, 209, 91, 18, 195, 173, 34, 52, 174, 25, 225, 146, 151, 39, 142, 143, 69, 165, 151, 227, 58, 19, 173, 48, 117, 36, 192, 212, 72, 78)
    }
  }

  "Marr hashes" should "compare not equal" in {
    val example2 = ImageIO.read(new File("src/test/resources/example2.jpg"))
    val example3 = ImageIO.read(new File("src/test/resources/example3.jpg"))

    for {
      example2MarrHash <- PHash.marrHash(example2, 4)
      example3MarrHash <- PHash.marrHash(example3, 4)
    } yield {
      PHash.marrHashDistance(example2MarrHash, example3MarrHash) shouldEqual Some(0.3993055555555556)
    }
  }

  "Marr hashes" should "compare equal" in {
    val example2 = ImageIO.read(new File("src/test/resources/example2.jpg"))
    val example4 = ImageIO.read(new File("src/test/resources/example4.jpg"))

    for {
      example2MarrHash <- PHash.marrHash(example2, 4)
      example4MarrHash <- PHash.marrHash(example4, 4)
    } yield {
      PHash.marrHashDistance(example2MarrHash, example4MarrHash) shouldEqual Some(0.3576388888888889)
    }
  }

  "PHash" should "compute radial hashes" in {
    val example2 = ImageIO.read(new File("src/test/resources/example2.jpg"))

    val example2DctHash = PHash.radialHash(example2)

    example2DctHash shouldEqual Array(196, 193, 0, 205, 90, 211, 195, 166, 255, 213, 189, 209, 188, 191, 177, 204, 180, 184, 192, 200, 189, 192, 196, 198, 196, 203, 201, 191, 189, 199, 195, 194, 199, 192, 196, 196, 198, 196, 193, 197)
  }

  "Radial hash" should "compare not equal" in {
    val example2 = ImageIO.read(new File("src/test/resources/example2.jpg"))
    val example3 = ImageIO.read(new File("src/test/resources/example3.jpg"))

    val example2RadialHash = PHash.radialHash(example2)
    val example3RadialHash = PHash.radialHash(example3)

    PHash.radialHashDistance(example2RadialHash, example3RadialHash) shouldEqual 0.8707074916650039
  }

  "Radial hash" should "compare equal" in {
    val example2 = ImageIO.read(new File("src/test/resources/example2.jpg"))
    val example4 = ImageIO.read(new File("src/test/resources/example4.jpg"))

    val example2RadialHash = PHash.radialHash(example2)
    val example4RadialHash = PHash.radialHash(example4)

    PHash.radialHashDistance(example2RadialHash, example4RadialHash) shouldEqual 0.9560809340807257
  }

  def approximatelyEqual(x: Float, y: Float, delta: Float): Boolean = {
    y - delta <= x && x <= y + delta
  }
}
