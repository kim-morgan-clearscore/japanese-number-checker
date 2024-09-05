ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.3"

libraryDependencies += "org.scalameta" %% "munit" % "1.0.0" % Test

lazy val root = (project in file("."))
  .settings(
    name := "japanese-number-checker"
  )
