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

@RunWith(value=classOf[org.junit.runners.Suite])
@SuiteClasses(value=Array(
    classOf[UnfilteredTest0],
    classOf[UnfilteredTest0a],
    classOf[UnfilteredTest1],
    classOf[UnfilteredTest2],
    classOf[UnfilteredTest2a],
    classOf[UnfilteredTest3a],
    classOf[UnfilteredTest3b],
    classOf[AmbiguityTest],
    classOf[UnfilteredTest]))
class UnfilteredSuite 

@RunWith(classOf[JUnitRunner])
class UnfilteredTest0 extends FunsuiteServed with Matchers {

  val Pth1 = RelativePath / "a"  
  val Pth2:PathDecoder[Path] = RelativePath / "bb" / "cc"
  val Pth3 = RelativePath && ("a", "bb")

  def setup = _.plan(Planify {
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
class UnfilteredTest0a extends FunsuiteServed with Matchers {

  val Pth1 = RelativePath / "a"
  val Pth2 = RelativePath / "bb" / "cc"
  val Pth3 = RelativePath && ("a", "bb")

  def setup = _.plan(Planify {
    case GET(Pth1.Partial(_)) => ResponseString("1") ~> Ok
    case GET(Pth2.Partial(_)) => ResponseString("2") ~> Ok
    case GET(Pth3.Partial(_)) => ResponseString("3") ~> Ok
  })

  test("pth1") {
    println(1)
    http(host / "a" as_str) should be("1")
    println(2)
    http(host / "bb" / "cc" as_str) should be("2")
    println(3)
    http(host <<? Map("a" -> "bb") as_str) should be("3")

    println(4)
    http(host / "a" / "extra" as_str) should be("1")
    println(5)
    http(host / "bb" / "cc" <<? Map("extra" -> "v") as_str) should be("2")
    println(6)
    http(host <<? Map("a" -> "bb", "extra" -> "v") as_str) should be("3")
  }

}

@RunWith(classOf[JUnitRunner])
class UnfilteredTest extends FunsuiteServed with Matchers {

  val Pth1 = RelativePath / Segment.string

  def setup = _.plan(Planify {
    case GET(Pth1("aaa")) => ResponseString("aaa") ~> Ok
    case GET(Pth1(v)) => ResponseString(s"test:$v") ~> Ok
  })

  test("pth1") {
    http(host / "aaa" as_str) should be("aaa")
    http(host / "bbb" as_str) should be("test:bbb")

    assertCode(404)(http(host / "bbb" / "extra" as_str))
  }

}

@RunWith(classOf[JUnitRunner])
class UnfilteredTest1 extends FunsuiteServed with Matchers {

  val Pth1 = RelativePath / Segment.string
  val Pth2 = Pth1 / "abba" / Segment.string 

  def setup = _.plan(Planify {
    case GET(Pth2(v1,v2)) => ResponseString(s"$v1:$v2") ~> Ok
    case GET(Pth2.Partial(v,v1)) => ResponseString(s"partial:$v:$v1") ~> Ok
    case GET(Pth1.Partial(v)) => ResponseString(s"partial:$v") ~> Ok
  })

  test("pth1 partial") {
    http(host / "aaa" as_str) should be("partial:aaa")
    http(host / "bbb" / "abba" as_str) should be("partial:bbb")
    http(host / "bbb" / "abba" / "ccc" / "ddd" as_str) should be("partial:bbb:ccc")
  }

}

@RunWith(classOf[JUnitRunner])
class UnfilteredTest2a extends FunsuiteServed with Matchers {

  val Pth2 = RelativePath / Segment.string / "abba" / Segment.string

  def setup = _.plan(Planify {
    case GET(Pth2((a, b))) => ResponseString(s"test:$a:$b") ~> Ok
  })

  test("pth2") {
    assertCode(404)(http(host / "aaa" as_str))
    http(host / "bbb" / "abba" / "ccc" as_str) should be("test:bbb:ccc")
    assertCode(404)(http(host / "bbb" / "abba" / "ccc" / "ddd" as_str))
  }

}

@RunWith(classOf[JUnitRunner])
class UnfilteredTest2 extends FunsuiteServed with Matchers {

  val Pth2 = RelativePath / Segment.string / "abba" / Segment.string 

  def setup = _.plan(Planify {
    case GET(Pth2.Partial((a, b))) => ResponseString(s"partial:$a:$b") ~> Ok
  })

  test("pth2 partial") {
    assertCode(404)(http(host / "aaa" as_str))
    http(host / "bbb" / "abba" / "ccc" as_str) should be("partial:bbb:ccc")
    http(host / "bbb" / "abba" / "ccc" / "ddd" as_str) should be("partial:bbb:ccc")
  }

}

@RunWith(classOf[JUnitRunner])
class UnfilteredTest3a extends FunsuiteServed with Matchers {

  val Pth2 = RelativePath / Segment.string / "abba" && Param.string("bb")

  def setup = _.plan(Planify {
    case GET(Pth2((a, b))) => ResponseString(s"test:$a:$b") ~> Ok
  })

  test("pth3 ") {
    assertCode(404)(http(host / "aaa" as_str))
    assertCode(404)(http(host / "bbb" / "abba" / "ccc" as_str))
    assertCode(404)(http(host / "bbb" / "abba" / "ccc" / "ddd" as_str))

    http(host / "bbb" / "abba" <<? Map("bb" -> "BB") as_str) should be("test:bbb:BB")

    assertCode(404)(http(host / "bbb" / "abba" <<? Map("bbb" -> "BB") as_str))
    assertCode(404)(http(host / "bbb" / "abba" <<? Map("bb" -> "BB", "ccc" -> "ccc") as_str))
  }

}

@RunWith(classOf[JUnitRunner])
class UnfilteredTest3b extends FunsuiteServed with Matchers {

  val Pth2 = RelativePath / Segment.string / "abba" && Param.string("bb")

  def setup = _.plan(Planify {
    case GET(Pth2.Partial((a, b))) => ResponseString(s"test:$a:$b") ~> Ok
  })

  test("pth3 partial") {
    assertCode(404)(http(host / "aaa" as_str))
    assertCode(404)(http(host / "bbb" / "abba" / "ccc" as_str))
    assertCode(404)(http(host / "bbb" / "abba" / "ccc" / "ddd" as_str))

    http(host / "bbb" / "abba" <<? Map("bb" -> "BB") as_str) should be("test:bbb:BB")
    http(host / "bbb" / "abba" <<? Map("bb" -> "BB", "cc" -> "bvbv") as_str) should be("test:bbb:BB")

    assertCode(404)(http(host / "bbb" / "abba" <<? Map("bbb" -> "BB") as_str))
  }

}

@RunWith(classOf[JUnitRunner])
class AmbiguityTest extends FunsuiteServed with Matchers {

  val Pth2 = RelativePath / Segment.string / "abba" && Param.string("bb")
  val Pth2a = RelativePath / Segment.int / "abba" && Param.string("bb")
  val Pth2a1 = RelativePath / Segment.int / "abba" && Param.int("bb")
  val Pth2b = RelativePath / Segment.boolean / "abba" && Param.string("bb")

  def setup = _.plan(Planify {
    case GET(Pth2b((a, b))) => ResponseString(s"2b:$a:$b") ~> Ok  
    case GET(Pth2a1((a, b))) => ResponseString(s"2a1:$a:$b") ~> Ok  
    case GET(Pth2a((a, b))) => ResponseString(s"2a:$a:$b") ~> Ok  
    case GET(Pth2((a, b))) => ResponseString(s"2:$a:$b") ~> Ok
  })

  test("pth3 partial") {
    assertCode(404)(http(host / "aaa" as_str))
    assertCode(404)(http(host / "bbb" / "abba" / "ccc" as_str))
    assertCode(404)(http(host / "bbb" / "abba" / "ccc" / "ddd" as_str))

    http(host / "bbb" / "abba" <<? Map("bb" -> "BB") as_str) should be("2:bbb:BB")
    http(host / "111" / "abba" <<? Map("bb" -> "BB") as_str) should be("2a:111:BB")
    http(host / "111" / "abba" <<? Map("bb" -> "232") as_str) should be("2a1:111:232")
    http(host / "true" / "abba" <<? Map("bb" -> "232") as_str) should be("2b:true:232")
    assertCode(404)(http(host / "bbb" / "abba" <<? Map("bb" -> "BB", "cc" -> "bvbv") as_str))

    assertCode(404)(http(host / "bbb" / "abba" <<? Map("bbb" -> "BB") as_str))
  }

}
