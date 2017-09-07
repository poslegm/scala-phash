name := "scala-phash"

organization := "com.github.poslegm"

version := "1.0.1"

scalaVersion := "2.12.1"
crossScalaVersions := Seq("2.11.8", "2.12.1")

resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies ++= Seq(
  "javax.media" % "jai-core" % "1.1.3" from "https://repository.jboss.org/nexus/content/repositories/thirdparty-releases/javax/media/jai-core/1.1.3/jai-core-1.1.3.jar",
  "org.scalactic" %% "scalactic" % "3.0.1",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test",
  "com.jhlabs" % "filters" % "2.0.235-1"
)

fork := true

javaOptions in test += "-Xms512M -Xmx1024M -Xss1M -XX:+CMSClassUnloadingEnabled -XX:MaxPermSize=256M"
