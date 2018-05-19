# Scala pHash

Scala fork of [pHash](http://phash.org) library. This library identifies whether images are similar.  
Original pHash uses CImg library for image processing but I could not find CImg for jvm. Therefore I use ```java.awt``` and self-made functions for image processing. Consequently, results of my library is different from original phash.
## How to use
My library implements three Perceptual Hashing algorithms: Radial Hash, DCT hash and Marr hash. [More info about it](http://www.phash.org/docs/pubs/thesis_zauner.pdf).

#### sbt dependencies

```scala 
resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
libraryDependencies += "com.github.poslegm" %% "scala-phash" % "1.1.0"
```

#### Example

For full public API with documentation see [`trait PHashAlgebra`](https://github.com/poslegm/scala-phash/blob/master/src/main/scala/com/github/poslegm/scalaphash/PHashAlgebra.scala).

```scala
import com.github.poslegm.scalaphash.PHash._
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
**DCT** and **Marr** distances are _less_ when images are similar.

<img width='200px' src='https://github.com/poslegm/scala-phash/blob/master/src/test/resources/example2.jpg'>
<img width='200px' src='https://github.com/poslegm/scala-phash/blob/master/src/test/resources/example4.jpg'>

```
radial: 0.9538751316650709
dct: 1
marr: 0.3315972222222222
```

<img width='200px' src='https://github.com/poslegm/scala-phash/blob/master/src/test/resources/1.jpg'>
<img width='200px' src='https://github.com/poslegm/scala-phash/blob/master/src/test/resources/2.jpg'>

```
radial: 0.36438994709451805
dct: 36
marr: 0.4947916666666667
```

## Warning
My results is not compatible with original pHash. Use original library if you have an opportunity.  
Also, it works **much** slower than c++ version (about 5-7 times).
## Thanks
* [pHash](http://phash.org) / [https://github.com/clearscene/pHash](https://github.com/clearscene/pHash) 
* [jphash](https://github.com/pragone/jphash)
