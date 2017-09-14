# Scala pHash

Scala fork of [pHash](http://phash.org) library. This library identifies whether images are similar.  
Original pHash uses CImg library for image processing but I could not find CImg for jvm. Therefore I use ```java.awt``` and self-made functions for image processing. Consequently, results of my library is different from original phash.
## How to use
My library implements three Perceptual Hashing algorithms: Radial Hash, DCT hash and Marr hash. [More info about it](http://www.phash.org/docs/pubs/thesis_zauner.pdf).

#### sbt dependencies

```scala 
libraryDependencies += "com.github.poslegm" %% "scala-phash" % "1.0.2"
```

#### Example

```scala
import com.github.poslegm.scalaphash.PHash
import javax.imageio.ImageIO

val img1 = ImageIO.read(new File("img1.jpg"))
val img2 = ImageIO.read(new File("img2.jpg"))

val img1rad = PHash.radialHash(img1)
val img2rad = PHash.radialHash(img2)

val radialDistance = PHash.radialHashDistance(img1rad, img2rad)

if (radialDistance > 0.95) {
  println("similar")
} else {
  println("not similar")
}
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
