import Dependencies._

organization in ThisBuild := "com.github.gdefacci"
version in ThisBuild := "0.9.0-SNAPSHOT"

crossScalaVersions := Seq(scala_2_11, scala_2_10, scala_2_12)

lazy val raz = Project(
  "raz",
  file("raz"),
  settings = Seq(
    libraryDependencies += scalazCore,
    libraryDependencies += shapeless,
    libraryDependencies += scalatest % "test"
  )
)

lazy val razHttp4s = Project(
  "raz-http4s",
  file("raz-http4s"),
  settings = Seq(
    libraryDependencies += http4s
  )
).dependsOn(raz)

lazy val root = (project in file(".")).
  aggregate(raz, razHttp4s).
  settings(
    publishArtifact := false
  )
  