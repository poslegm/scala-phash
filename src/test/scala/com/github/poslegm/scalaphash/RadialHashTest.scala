package com.github.poslegm.scalaphash

import java.io.File
import javax.imageio.ImageIO

import org.scalatest.{FlatSpec, Matchers, PrivateMethodTester}

class RadialHashTest extends FlatSpec with Matchers with PrivateMethodTester {
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
}
