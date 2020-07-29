import Dependencies._

ThisBuild / scalaVersion     := "2.13.2"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "dev.ostrander"
ThisBuild / organizationName := "Ostrander"

lazy val root = (project in file("."))
  .settings(
    name := "discpardy",
    resolvers += Resolver.JCenterRepository,
    libraryDependencies ++= List(   
      "net.katsstuff" %% "ackcord" % "0.16.1",
      "io.spray" %%  "spray-json" % "1.3.5",
      scalaTest % Test,
    )
  )
