package org.obl.raz.plans

import unfiltered.filter.Planify
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import unfiltered.response._
import unfiltered.request._
import unfiltered.request.{ Path => UPath }
import dispatch.classic._
import org.obl.raz._
import org.obl.raz
import org.junit.runners.Suite.SuiteClasses
import org.scalatest.Matchers

import org.obl.raz.unfiltered.Unfiltered
import org.obl.raz.unfiltered.Unfiltered._

import PathConverter._

import scala.language.postfixOps 

@RunWith(classOf[JUnitRunner])
class MatchAbsoluteTest extends FunsuiteServed with Matchers {

  var base = raz.HTTP("mysite.com")
  
  val Pth1 = base / "a"
  val Pth2 = base / "bb" / "cc"
  val Pth3 = base && ("a", "bb")

  def setup = _.plan(Planify  {
    case GET(Pth1(_)) => ResponseString("1") ~> Ok
    case GET(Pth2(_)) => ResponseString("2") ~> Ok
    case GET(Pth3(_)) => ResponseString("3") ~> Ok
  })

  test("pth1") {
    assertCode(404)(http(host / "bb" / "cc" <<? Map("extra" -> "v") as_str))
    
    http(host <<? Map("a" -> "bb") as_str) should be("3")
    http(host / "a" as_str) should be("1")

    http(host / "bb" / "cc" as_str) should be("2")

    assertCode(404)(http(host / "a" / "extra" as_str))
    assertCode(404)(http(host <<? Map("a" -> "bb", "extra" -> "v") as_str))


  }

}


@RunWith(classOf[JUnitRunner])
class MatchAbsoluteTest1 extends FunsuiteServed with Matchers {

  var base = raz.HTTP("mysite.com")
  
  val Pth2 = raz.HTTP("mysite.com") / Segment.string / "abba" / Segment.string

  def setup = _.plan(Planify {
    case GET(Pth2.Partial((a, b))) => ResponseString(s"partial:$a:$b") ~> Ok
  })

  test("pth2 partial") {
    assertCode(404)(http(host / "aaa" as_str))
    http(host / "bbb" / "abba" / "ccc" as_str) should be("partial:bbb:ccc")
    http(host / "bbb" / "abba" / "ccc" / "ddd" as_str) should be("partial:bbb:ccc")
  }

}
