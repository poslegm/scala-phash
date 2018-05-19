name := "scala-phash"

organization := "com.github.poslegm"

version := "1.1.0"

scalaVersion := "2.12.6"
crossScalaVersions := Seq("2.11.8", "2.12.1", scalaVersion.value)

resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies ++= Seq(
  "org.scalactic" %% "scalactic" % "3.0.1",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test",
  "com.jhlabs" % "filters" % "2.0.235-1"
)

fork := true

javaOptions in test += "-Xms512M -Xmx1024M -Xss1M -XX:+CMSClassUnloadingEnabled -XX:MaxPermSize=256M"
