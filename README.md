# Scala pHash

[![Build Status](https://travis-ci.org/poslegm/scala-phash.svg?branch=master)](https://travis-ci.org/poslegm/scala-phash)
[![Maven](https://img.shields.io/maven-central/v/com.github.poslegm/scala-phash_2.12.svg)](https://search.maven.org/artifact/com.github.poslegm/scala-phash_2.12/)

Scala fork of [pHash](http://phash.org) library. This library identifies whether images are similar. You can try it at [demo page](https://chugunkov.website/scala-phash-demo).  

Original pHash uses CImg library for image processing but I could not find CImg for jvm. Therefore I use ```java.awt``` and self-made functions for image processing. Consequently, results of my library is different from original phash.

## How to use
My library implements three Perceptual Hashing algorithms: Radial Hash, DCT hash and Marr hash. [More info about it](http://www.phash.org/docs/pubs/thesis_zauner.pdf).

#### sbt dependencies

```scala 
resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
libraryDependencies += "com.github.poslegm" %% "scala-phash" % "1.1.1"
```

#### API
There is three functions for each hashing algorithm. Let's consider them by example of DCT hash:
* `def dctHash(image: BufferedImage): Either[Throwable, DCTHash]` ― compute image's hash;
* `def unsafeDctHash(image: BufferedImage): DCTHash` ― compute image's hash unsafely (danger of exception);
* `def dctHashDistance(hash1: DCTHash, hash2: DCTHash): Long` ― compare hashes of two images.

Similar functions written for Marr and Radial Hash algorithms.

All public api with scaladocs decsribed in object [`PHash`](https://github.com/poslegm/scala-phash/blob/master/src/main/scala/com/github/poslegm/scalaphash/PHash.scala).

#### Example

```scala
import scalaphash.PHash._
import javax.imageio.ImageIO

val img1 = ImageIO.read(new File("img1.jpg"))
val img2 = ImageIO.read(new File("img2.jpg"))

val radialDistance: Either[Throwable, Double] = for {
  img1rad <- radialHash(img1)
  img2rad <- radialHash(img2)
} yield radialHashDistance(img1rad, img2rad)

radialDistance.foreach {
  case distance if distance > 0.95 => println("similar")
  case _ => println("not similar")
}

radialDistance.left.foreach(e => println(e.getMessage))
```

**Radial** distance is _more_ when images are similar.
**DCT** and **Marr** distances are _less_ when images are similar. Recommended to make a decision on image similarity when at least two hashes pass thresholds.

<img width='200px' src='https://github.com/poslegm/scala-phash/blob/master/src/test/resources/lenna.jpg'>
<img width='200px' src='https://github.com/poslegm/scala-phash/blob/master/src/test/resources/lenna2.jpg'>

```
radial: 0.9508017124330319
dct: 13
marr: 0.5052083333333334
```

<img width='200px' src='https://github.com/poslegm/scala-phash/blob/master/src/test/resources/1.jpg'>
<img width='200px' src='https://github.com/poslegm/scala-phash/blob/master/src/test/resources/2.jpg'>

```
radial: 0.3996241672331173
dct: 41
marr: 0.4704861111111111
```

## Warning
My results is not compatible with original pHash. Use original library if you have an opportunity.  
Also, it works **much** slower than c++ version (about 5-7 times).
## Thanks
* [pHash](http://phash.org) / [https://github.com/clearscene/pHash](https://github.com/clearscene/pHash) 
* [jphash](https://github.com/pragone/jphash)
