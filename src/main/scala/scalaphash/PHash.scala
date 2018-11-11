package scalaphash

import java.awt.image.BufferedImage

import scala.util.control.NonFatal

object PHash {
  type DCTHash = Long
  type MarrHash = Array[Int]
  type RadialHash = Array[Int]

  /**
    * Computes DCT hash value of image
    * (http://www.phash.org/docs/pubs/thesis_zauner.pdf / page 21)
    * @param image image for hashing
    * @return 64-bit hash value or exception
    * */
  def dctHash(image: BufferedImage): Either[Throwable, DCTHash] =
    try Right(unsafeDctHash(image))
    catch { case NonFatal(e) => Left(e) }

  /**
    * Computes DCT hash value of image
    * (http://www.phash.org/docs/pubs/thesis_zauner.pdf / page 21)
    * @param image image for hashing
    * @return 64-bit hash value
    * */
  def unsafeDctHash(image: BufferedImage): DCTHash = PHashInternal.unsafeDctHash(image)

  /**
    * Computes distance between two DCT hashes
    * Less is better
    * */
  def dctHashDistance(hash1: DCTHash, hash2: DCTHash): Long = PHashInternal.dctHashDistance(hash1, hash2)

  /**
    * Computes Marr hash value of image
    * (http://www.phash.org/docs/pubs/thesis_zauner.pdf / page 22)
    * @param image image for hashing
    * @param alpha coefficient for correlation kernel
    * @param level coefficient for correlation kernel
    * @return hash as int array or exception
    * */
  def marrHash(image: BufferedImage, alpha: Int = 2, level: Int = 1): Either[Throwable, MarrHash] =
    try Right(unsafeMarrHash(image, alpha, level))
    catch { case NonFatal(e) => Left(e) }

  /**
    * Computes Marr hash value of image
    * (http://www.phash.org/docs/pubs/thesis_zauner.pdf / page 22)
    * @param image image for hashing
    * @param alpha coefficient for correlation kernel
    * @param level coefficient for correlation kernel
    * @return hash as int array
    * */
  def unsafeMarrHash(image: BufferedImage, alpha: Int = 2, level: Int = 1): MarrHash =
    PHashInternal.unsafeMarrHash(image, alpha, level)

  /**
    * Computes distance between two Marr hashes
    * Less is better
    * */
  def marrHashDistance(hash1: MarrHash, hash2: MarrHash): Option[Double] = PHashInternal.marrHashDistance(hash1, hash2)

  /**
    * Computes Radial hash value of image
    * (http://www.phash.org/docs/pubs/thesis_zauner.pdf / page 24)
    * @param image image for hashing
    * @param projectionsCount number of projections to compute
    * @return hash as int array or exception
    * */
  def radialHash(image: BufferedImage, projectionsCount: Int = 180): Either[Throwable, RadialHash] =
    try Right(unsafeRadialHash(image, projectionsCount))
    catch { case NonFatal(e) => Left(e) }

  /**
    * Computes Radial hash value of image
    * (http://www.phash.org/docs/pubs/thesis_zauner.pdf / page 24)
    * @param image image for hashing
    * @param projectionsCount number of projections to compute
    * @return hash as int array
    * */
  def unsafeRadialHash(image: BufferedImage, projectionsCount: Int = 180): RadialHash =
    PHashInternal.unsafeRadialHash(image, projectionsCount)

  /**
    * Computes distance between two Radial hashes
    * More is better
    * */
  def radialHashDistance(hash1: RadialHash, hash2: RadialHash): Double = PHashInternal.radialHashDistance(hash1, hash2)
}
