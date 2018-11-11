package com.github.poslegm.scalaphash

import java.io.File
import javax.imageio.ImageIO

import org.scalatest.{FlatSpec, Matchers, PrivateMethodTester}

class RadialHashTest extends FlatSpec with Matchers with PrivateMethodTester {
  "PHash" should "compute radial hashes" in {
    val bag = ImageIO.read(new File("src/test/resources/bag1.jpg"))

    val example2RadialHash = PHash.radialHash(bag)

    example2RadialHash.right.get shouldEqual Array(193, 190, 0, 204, 92, 209, 183, 165, 254, 211, 188, 209, 182, 185,
      176, 203, 182, 178, 190, 196, 187, 192, 197, 193, 192, 199, 200, 189, 186, 195, 191, 192, 198, 191, 192, 192, 195,
      195, 192, 194)
  }

  "Radial hash" should "compare not equal" in {
    val bag1 = ImageIO.read(new File("src/test/resources/bag1.jpg"))
    val bag2 = ImageIO.read(new File("src/test/resources/bag2.jpg"))

    (for {
      bag1RadialHash <- PHash.radialHash(bag1)
      bag2RadialHash <- PHash.radialHash(bag2)
    } yield PHash.radialHashDistance(bag1RadialHash, bag2RadialHash)) shouldEqual Right(0.8893141328103169)
  }

  "Radial hash" should "compare equal" in {
    val origin = ImageIO.read(new File("src/test/resources/lenna.jpg"))
    val modified = ImageIO.read(new File("src/test/resources/lenna2.jpg"))

    (for {
      originRadialHash <- PHash.radialHash(origin)
      modifiedRadialHash <- PHash.radialHash(modified)
    } yield PHash.radialHashDistance(originRadialHash, modifiedRadialHash)) shouldEqual Right(0.9508017124330319)
  }

  "Radial hash" should "compare nature" in {
    val a = ImageIO.read(new File("src/test/resources/mountain1.jpeg"))
    val b = ImageIO.read(new File("src/test/resources/mountain2.jpeg"))

    (for {
      aRadialHash <- PHash.radialHash(a)
      bRadialHash <- PHash.radialHash(b)
    } yield PHash.radialHashDistance(aRadialHash, bRadialHash)) shouldEqual Right(0.7168435425660051)
  }

  "Radial hash" should "compare dog and cat" in {
    val dog = ImageIO.read(new File("src/test/resources/1.jpg"))
    val cat = ImageIO.read(new File("src/test/resources/2.jpg"))

    (for {
      dogRadialHash <- PHash.radialHash(dog)
      catRadialHash <- PHash.radialHash(cat)
    } yield PHash.radialHashDistance(dogRadialHash, catRadialHash)) shouldEqual Right(0.3996241672331173)
  }
}
