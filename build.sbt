name := "circe-jsondiffpatch"

version := "0.14.1"

scalaVersion := "2.13.6"

val circeVersion = "0.14.1"
libraryDependencies ++= Seq(
  "io.circe" %% "circe-core",
).map(_ % circeVersion)


libraryDependencies += "org.bitbucket.cowwoc" % "diff-match-patch" % "1.2"