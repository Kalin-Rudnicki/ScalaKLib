import sbt.Keys.version

lazy val root = project
  .in(file("."))
  .settings(
    name := "KLib",
    version := "0.1",
    scalaVersion := "0.24.0-RC1",
    libraryDependencies ++=
      List(
        "org.scalaz" %% "scalaz-core" % "7.2.30",
        "org.scalactic" %% "scalactic" % "3.1.1",
        "org.scalatest" %% "scalatest" % "3.1.1" % "test",
        "org.scalameta" %% "scalameta" % "4.3.10"
      ).map(_.withDottyCompat(scalaVersion.value))
  )
