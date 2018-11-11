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
    val matrix = PHashInternal invokePrivate createDctMatrix(6)
    matrix.length shouldEqual canonical.length
    matrix(0).length shouldEqual canonical(0).length

    (for {
      i <- matrix.indices
      j <- matrix(i).indices
    } yield matrix(i)(j) ~= canonical(i)(j)).forall(x => x) should be(true)
  }

  "PHash" should "compute dct hashes" in {
    val bag = ImageIO.read(new File("src/test/resources/bag1.jpg"))

    PHash.dctHash(bag) shouldEqual Right(2113378283)
  }

  "DCT hash" should "compare not equal" in {
    val bag1 = ImageIO.read(new File("src/test/resources/bag1.jpg"))
    val bag2 = ImageIO.read(new File("src/test/resources/bag2.jpg"))

    (for {
      bag1DctHash <- PHash.dctHash(bag1)
      bag2DctHash <- PHash.dctHash(bag2)
    } yield PHash.dctHashDistance(bag1DctHash, bag2DctHash)) shouldEqual Right(7)
  }

  "DCT hash" should "compare dog and cat" in {
    val dog = ImageIO.read(new File("src/test/resources/1.jpg"))
    val cat = ImageIO.read(new File("src/test/resources/2.jpg"))

    (for {
      dogDctHash <- PHash.dctHash(dog)
      catDctHash <- PHash.dctHash(cat)
    } yield PHash.dctHashDistance(dogDctHash, catDctHash)) shouldEqual Right(41)
  }

  "DCT hash" should "compare nature" in {
    val mountain1 = ImageIO.read(new File("src/test/resources/mountain1.jpeg"))
    val mountain2 = ImageIO.read(new File("src/test/resources/mountain2.jpeg"))

    (for {
      mountain1DctHash <- PHash.dctHash(mountain1)
      mountain2DctHash <- PHash.dctHash(mountain2)
    } yield PHash.dctHashDistance(mountain1DctHash, mountain2DctHash)) shouldEqual Right(43)
  }

  "DCT hash" should "compare equal" in {
    val origin = ImageIO.read(new File("src/test/resources/lenna.jpg"))
    val modified = ImageIO.read(new File("src/test/resources/lenna2.jpg"))

    (for {
      originDctHash <- PHash.dctHash(origin)
      modifiedDctHash <- PHash.dctHash(modified)
    } yield PHash.dctHashDistance(originDctHash, modifiedDctHash)) shouldEqual Right(13)
  }
}
