import sbt._
import Keys._

object RazBuild extends Build {

  val scala_2_10 = "2.10.6"
  val scala_2_11 = "2.11.8"

  val buildScalaVersion = "2.11.8"

  val scalazVersion = "7.2.4"
  val http4sVersion = "0.14.1a"
  val shapelessVersion = "2.3.1"
  val scalatestVersion = "2.2.6"
  val scalacheckVersion = "1.13.2"

  val scalazCore = "org.scalaz" %% "scalaz-core" % scalazVersion

  val http4s = "org.http4s" %% "http4s-dsl" % http4sVersion

  val shapeless = "com.chuusai" %% "shapeless" % shapelessVersion

  val scalatest = "org.scalatest" %% "scalatest" % scalatestVersion

  val scalacheck = "org.scalacheck" %% "scalacheck" % scalacheckVersion

  def scalaReflect(version: String) = "org.scala-lang" % "scala-reflect" % version

}
