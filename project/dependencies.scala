import sbt._
import Keys._

object Dependencies {

  val scala_2_10 = "2.10.6"
  val scala_2_11 = "2.11.8"
  val scala_2_12 = "2.12.1"
  
  val scalazVersion = "7.2.8"
  val http4sVersion ="0.15.0"

  val shapelessVersion = "2.3.2"
  val scalatestVersion = "3.0.1"
  val scalacheckVersion = "1.13.2"

  val scalazCore = "org.scalaz" %% "scalaz-core" % scalazVersion

  val http4s = "org.http4s" %% "http4s-dsl" % http4sVersion

  val shapeless = "com.chuusai" %% "shapeless" % shapelessVersion

  val scalatest = "org.scalatest" %% "scalatest" % scalatestVersion

  val scalacheck = "org.scalacheck" %% "scalacheck" % scalacheckVersion

  def scalaReflect(version: String) = "org.scala-lang" % "scala-reflect" % version

}
