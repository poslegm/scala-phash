name := "scala-phash"

version := "1.0"

scalaVersion := "2.12.1"

libraryDependencies ++= Seq(
  "javax.media" % "jai_core" % "1.1.3" from "http://download.osgeo.org/webdav/geotools/javax/media/jai_core/1.1.3/jai_core-1.1.3.jar",
  "org.scalactic" %% "scalactic" % "3.0.1",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test",
  "com.sksamuel.scrimage" %% "scrimage-core" % "2.1.8",
  "com.sksamuel.scrimage" %% "scrimage-io-extra" % "2.1.8",
  "com.sksamuel.scrimage" %% "scrimage-filters" % "2.1.8"
)