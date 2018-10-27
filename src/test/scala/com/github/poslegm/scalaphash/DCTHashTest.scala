package com.github.poslegm.scalaphash

import java.io.File
import javax.imageio.ImageIO

import org.scalatest.{FlatSpec, Matchers, PrivateMethodTester}
import MathUtils._

class DCTHashTest extends FlatSpec with Matchers with PrivateMethodTester {
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
    } yield matrix(i)(j) ~= canonical(i)(j)).forall(x => x) should be(true)
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
}
