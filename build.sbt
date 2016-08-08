enablePlugins(CrossPerProjectPlugin)

organization in ThisBuild := "org.obl"
version in ThisBuild := "0.9.0-SNAPSHOT"

lazy val allScalaVersion = Seq(scala_2_11, scala_2_10)

lazy val raz = Project(
  "raz",
  file("raz"),
  settings = Seq(
    crossScalaVersions := allScalaVersion,
    libraryDependencies += scalazCore,
    libraryDependencies += shapeless,
    libraryDependencies += scalatest % "test"
  )
)

lazy val razHttp4s = Project(
  "raz-http4s",
  file("raz-http4s"),
  settings = Seq(
    crossScalaVersions := allScalaVersion,
    libraryDependencies += http4s
  )
).dependsOn(raz)

lazy val root = (project in file(".")).
  aggregate(raz, razHttp4s).
  settings(
    publishArtifact := false
  )
  