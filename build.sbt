import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.jambit",
      scalaVersion := "2.12.7",
      version      := "0.1.0-SNAPSHOT",
      scalacOptions += "-Ypartial-unification"
    )),
    name := "adventofcode2018",
    libraryDependencies += scalaTest % Test,
    libraryDependencies += "org.typelevel" %% "cats-core" % "1.5.0"
  )
