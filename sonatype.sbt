sonatypeProfileName := "poslegm"

publishMavenStyle := true

licenses := Seq("APL2" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt"))

homepage := Some(url("https://github.com/poslegm/scala-phash"))

scmInfo := Some(
  ScmInfo(
    url("https://github.com/poslegm/scala-phash"),
    "scm:git@github.com:poslegm/scala-phash.git"
  )
)

developers := List(
  Developer(id="poslegm", name="Mikhail Chugunkov", email="poslegm@gmail.com", url=url("https://github.com/poslegm"))
)

publishTo := Some(
  if (isSnapshot.value)
    Opts.resolver.sonatypeSnapshots
  else
    Opts.resolver.sonatypeStaging
)