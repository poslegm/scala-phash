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
    val example3 = ImageIO.read(new File("src/test/resources/example3.jpg"))
    val example4 = ImageIO.read(new File("src/test/resources/example4.jpg"))

    for {
      example2DctHash <- PHash.dctHash(example2)
      example3DctHash <- PHash.dctHash(example3)
      example4DctHash <- PHash.dctHash(example4)
    } yield {
      example2DctHash shouldEqual 1169849770
      PHash.dctHashDistance(example2DctHash, example3DctHash) shouldEqual 37
      PHash.dctHashDistance(example2DctHash, example4DctHash) shouldEqual 1
    }
  }

  "PHash" should "compute marr hashes" in {
    val example2 = ImageIO.read(new File("src/test/resources/example2.jpg"))
    val example3 = ImageIO.read(new File("src/test/resources/example3.jpg"))
    val example4 = ImageIO.read(new File("src/test/resources/example4.jpg"))

    for {
      example2DctHash <- PHash.marrHash(example2)
      example3DctHash <- PHash.marrHash(example3)
      example4DctHash <- PHash.marrHash(example4)
    } yield {
      example2DctHash shouldEqual Array(0, 0, 0, 56, 68, 200, 0, 0, 0, 0, 0, 0, 96, 0, 27, 36, 0, 0, 13, 166, 96, 141, 50, 219, 5, 50, 16, 184, 78, 17, 244, 194, 154, 106, 45, 11, 226, 228, 180, 147, 36, 208, 76, 209, 91, 18, 195, 173, 34, 52, 174, 25, 225, 146, 151, 39, 142, 143, 69, 165, 151, 227, 58, 19, 173, 48, 117, 36, 192, 212, 72, 78)
      PHash.marrHashDistance(example2DctHash, example3DctHash) shouldEqual Some(0.3993055555555556)
      PHash.marrHashDistance(example2DctHash, example4DctHash) shouldEqual Some(0.3576388888888889)
    }
  }

  "PHash" should "compute radial hashes" in {
    val example2 = ImageIO.read(new File("src/test/resources/example2.jpg"))
    val example3 = ImageIO.read(new File("src/test/resources/example3.jpg"))
    val example4 = ImageIO.read(new File("src/test/resources/example4.jpg"))

    val example2DctHash = PHash.radialHash(example2)
    val example3DctHash = PHash.radialHash(example3)
    val example4DctHash = PHash.radialHash(example4)

    example2DctHash shouldEqual Array(184, 168, 0, 200, 125, 184, 130, 181, 255, 199, 189, 220, 158, 163, 190, 194, 186, 169, 185, 182, 182, 193, 195, 175, 179, 187, 189, 186, 176, 181, 187, 190, 187, 182, 184, 180, 176, 188, 188, 180)
    PHash.radialHashDistance(example2DctHash, example3DctHash) shouldEqual 0.8949662148335105
    PHash.radialHashDistance(example2DctHash, example4DctHash) shouldEqual 0.9391380891489878
  }

  def approximatelyEqual(x: Float, y: Float, delta: Float): Boolean = {
    y - delta <= x && x <= y + delta
  }
}
