package com.github.poslegm.scalaphash

import java.awt.image.BufferedImage

trait PHashAlgebra {
  type DCTHash = Long
  type MarrHash = Array[Int]
  type RadialHash = Array[Int]

  /**
    * Computes DCT hash value of image
    * (http://www.phash.org/docs/pubs/thesis_zauner.pdf / page 21)
    * @param image image for hashing
    * @return 64-bit hash value or exception
    * */
  def dctHash(image: BufferedImage): Either[Throwable, DCTHash]

  /**
    * Computes DCT hash value of image
    * (http://www.phash.org/docs/pubs/thesis_zauner.pdf / page 21)
    * @param image image for hashing
    * @return 64-bit hash value
    * */
  def unsafeDctHash(image: BufferedImage): DCTHash

  /**
    * Computes distance between two DCT hashes
    * Less is better
    * */
  def dctHashDistance(hash1: DCTHash, hash2: DCTHash): Long

  /**
    * Computes Marr hash value of image
    * (http://www.phash.org/docs/pubs/thesis_zauner.pdf / page 22)
    * @param image image for hashing
    * @param alpha coefficient for correlation kernel
    * @param level coefficient for correlation kernel
    * @return hash as int array or exception
    * */
  def marrHash(image: BufferedImage, alpha: Int = 2, level: Int = 1): Either[Throwable, MarrHash]

  /**
    * Computes Marr hash value of image
    * (http://www.phash.org/docs/pubs/thesis_zauner.pdf / page 22)
    * @param image image for hashing
    * @param alpha coefficient for correlation kernel
    * @param level coefficient for correlation kernel
    * @return hash as int array
    * */
  def unsafeMarrHash(image: BufferedImage, alpha: Int = 2, level: Int = 1): MarrHash

  /**
    * Computes distance between two Marr hashes
    * Less is better
    * */
  def marrHashDistance(hash1: MarrHash, hash2: MarrHash): Option[Double]

  /**
    * Computes Radial hash value of image
    * (http://www.phash.org/docs/pubs/thesis_zauner.pdf / page 24)
    * @param image image for hashing
    * @param projectionsCount number of projections to compute
    * @return hash as int array or exception
    * */
  def radialHash(image: BufferedImage, projectionsCount: Int = 180): Either[Throwable, RadialHash]

  /**
    * Computes Radial hash value of image
    * (http://www.phash.org/docs/pubs/thesis_zauner.pdf / page 24)
    * @param image image for hashing
    * @param projectionsCount number of projections to compute
    * @return hash as int array
    * */
  def unsafeRadialHash(image: BufferedImage, projectionsCount: Int = 180): RadialHash

  /**
    * Computes distance between two Radial hashes
    * More is better
    * */
  def radialHashDistance(hash1: RadialHash, hash2: RadialHash): Double
}
