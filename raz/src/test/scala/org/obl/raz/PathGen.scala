package org.obl.raz

import org.scalacheck._
import org.scalacheck.Prop._
import org.scalacheck.Arbitrary.arbitrary
import scalaz.{ -\/, \/, \/- }
import java.net.URLDecoder

object PathGen extends Properties("path") {

  lazy val hostChar = Gen.oneOf('a'.to('z') ++ 'A'.to('Z'))

  lazy val urlCharGen = Gen.oneOf(hostChar, Gen.numChar, Gen.oneOf(
    '-', '_', '.', '~', '!', '*', '\'', '(', ')', ';', ':', '@', '&', '=', '+', '$', ',', '/', '?', '%', '#', '[', ']', ' '))

  lazy val urlStringGen = Gen.nonEmptyListOf(urlCharGen).map(_.mkString)

  implicit val arbitraryString = Arbitrary(urlStringGen)

  lazy val genProtocol = Gen.oneOf(HTTP, HTTPS)

  lazy val genHost = for {
    host <- Gen.nonEmptyListOf(hostChar).map(_.mkString)
    domain <- Gen.oneOf("com", "io", "net", "org")
  } yield s"$host.$domain"

  lazy val genAutority = for {
    host <- genHost
    port <- Gen.posNum[Int]
  } yield Authority(host, port)

  lazy val genPath = for {
    protocol <- Gen.option(genProtocol)
    authority <- Gen.option(genAutority)
    segments <- arbitrary[List[String]]
    pars <- arbitrary[List[(String, Option[String])]]
    fragment <- arbitrary[Option[String]]
  } yield Path(protocol, authority, segments, pars, fragment)

  lazy val genUrlPath = for {
    protocol <- genProtocol
    authority <- genAutority
    segments <- arbitrary[List[String]]
    pars <- arbitrary[List[(String, Option[String])]]
    fragment <- arbitrary[Option[String]]
  } yield Path(Some(protocol), Some(authority), segments, pars, fragment)

  lazy val genUriPath = for {
    protocol <- Gen.oneOf(HTTP, HTTPS, WS)
    authority <- genAutority
    segments <- Gen.listOf(Gen.nonEmptyListOf(Gen.alphaNumChar).map(_.mkString))
    pars <- arbitrary[List[(String, Option[String])]]
    fragment <- arbitrary[Option[String]]
  } yield Path(Some(protocol), Some(authority), segments, pars, fragment)

  implicit val arbitraryPath = Arbitrary(genPath)

  property("PathDecoder.path") = forAll { p: Path =>
    PathDecoder.path(p).decodeFull(p) == \/-(p)
  }

  {
    implicit val arbitraryPath = Arbitrary(genUriPath)

    property("java uri") = forAll { p: Path =>
      val pstr = p.render
      val uri = \/.fromTryCatchNonFatal(new java.net.URI(pstr))
      uri.map(uri => Path.fromJavaUri(uri) == \/-(p)).getOrElse(false)
    }
  }

  {
    implicit val arbitraryPath = Arbitrary(genUrlPath)

    property("java url") = forAll { p: Path =>
      val jurl = Path.toJavaUrl(p)
      Path.fromJavaUrl(jurl).map { p1 =>
        val r = p == p1
        val sameToString = jurl.toString == p1.render
        r && sameToString
      }.getOrElse(false)
    }
  }
}