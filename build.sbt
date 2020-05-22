libraryDependencies ++= List(
  "org.scalaz" %% "scalaz-core" % "7.2.30",
  "org.scalactic" %% "scalactic" % "3.1.1",
  "org.scalatest" %% "scalatest" % "3.1.1" % "test",
  "org.scalameta" %% "scalameta" % "4.3.10"
)

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full)

name := "KLib"
version := "0.1"
scalaVersion := "2.13.2"
