ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.10"
libraryDependencies += "com.github.tototoshi" %% "scala-csv" % "1.3.6"
libraryDependencies += "com.github.pathikrit" %% "better-files" % "3.9.1"

lazy val root = (project in file("."))
  .settings(
    name := "energy system"
  )
