package org.obl.raz.plans

import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import unfiltered.filter.Planify
import unfiltered.response._
import unfiltered.request._
import unfiltered.request.{ Path => UPath }
import dispatch.classic._
import org.obl.raz._
import org.junit.runners.Suite.SuiteClasses
import org.scalatest.Matchers

import PathConverter._

import org.obl.raz.unfiltered.Unfiltered
import org.obl.raz.unfiltered.Unfiltered._
import scala.language.postfixOps 

@RunWith(classOf[JUnitRunner])
class SingleTest extends FunsuiteServed with Matchers {

  val Pth1 = RelativePath / "a"
//  val Pth2 = RelativePath / "bb" / "cc"
//  val Pth3 = RelativePath && ("a", "bb")

  def setup = _.plan(Planify {
    case GET(Pth1.Partial(_)) => ResponseString("1") ~> Ok
//    case GET(Pth2.Partial(_)) => ResponseString("2") ~> Ok
//    case GET(Pth3.Partial(_)) => ResponseString("3") ~> Ok
  })

  test("pth1") {
//    println(1)
//    http(host / "a" as_str) should be("1")
//    println(2)
//    http(host / "bb" / "cc" as_str) should be("2")
//    println(3)
//    http(host <<? Map("a" -> "bb") as_str) should be("3")

    println(4)
    http(host / "a" / "extra" as_str) should be("1")
    println(5)
//    http(host / "bb" / "cc" <<? Map("extra" -> "v") as_str) should be("2")
//    println(6)
//    http(host <<? Map("a" -> "bb", "extra" -> "v") as_str) should be("3")
  }

}
