package org.obl.raz

import org.junit._
import org.junit.Assert._

import scalaz.{-\/, \/, \/-}

class MatcherTest {

  import PathConverter._

  @Test
  def testMatcher1 = {

    val u2 = RelativePath.add("a").add("b").add(Segment.int).add("c").addParam(Param.string("aaa"))

    val mtchr = u2.decodeFull(u2(10, "bb")).toOption

    mtchr match {
      case Some((10, "bb")) => ()
      case x => {
        println(x)
        fail("matcher " + mtchr)
      }
    }
  }

  @Test
  def testMatcher2 = {

    val u2 = RelativePath.add("a").add("b").add(Segment.int).add("c").addParam(Param.string("aaa"))
    val u3 = u2.addParam("d", "eeee")

    val mtchr1 = u2.decode(u3(11, "bubu"))

    mtchr1 match {
      case \/-(PathMatchResult((11, "bubu"), Path(None, PathSg.empty, Seq(QParamSg("d", Some("eeee"))), None))) => ()
      case x => fail("matcher " + mtchr1)
    }

    mtchr1 match {
      case \/-(PathMatchResult(res, Path(None, PathSg.empty, Seq(QParamSg("d", Some("eeee"))), None))) => {
        assert(res._1 == 11)
      }
      case x => fail("matcher " + mtchr1)
    }
  }

  @Test
  def optTest = {

    val u = RelativePath.add("a").add("b").add(Segment.string).addParam(opt(Param.int("opt")))

    val u1 = u("bbbb", Some(3))
    println(u("bbb", Some(3)).render)
    println(u.decodeFull(u1))

    Assert.assertEquals("bbbb" -> Some(3), u.decodeFull(u1).toOption.get)
  }

  @Test
  def noneTest0 = {
    val u = RelativePath && opt(Param.int("opt")) && opt(Param.string("opt"))
    val pmr = u.decode( RelativePath && ("opt", "") )
    
    assert(pmr.toOption.get.value == (None, Some("")))
    assert(pmr.toOption.get.rest.isEmpty)
    
  }

  @Test
  def noneTest = {

    val u = RelativePath.add("a").add("b").add(Segment.string).addParam(opt(Param.int("opt")))
    val u1 = u("bbbb", None)

    Assert.assertEquals("bbbb" -> None, u.decodeFull(u1).toOption.get)
    
    val pth1 = RelativePath / "a" / "b" / "c" && ("opt", "")

    Assert.assertEquals("c" -> None, u.decode(pth1).toOption.get.value)
    
    Assert.assertTrue(u.decode(pth1).toOption.get.rest == RelativePath.addParam("opt", "") )
  }

  case class Cl1(a: Int, b: String)

  @Test
  def testMapAndMatch = {
//    val sfx = (RelativePath && Param.int("a") && Param.string("b")).mapTo(Converter.tryConverter(Cl1.tupled, Cl1.unapply))
    val sfx = PathConverter(RelativePath && Param.int("a") && Param.string("b")).caseMap(Cl1.tupled, Cl1.unapply)
    val prfx = RelativePath / "h" / Segment.string

    val u1 = prfx && sfx
    assertEquals(u1.decodeFull(u1("a", Cl1(2, "bbaw"))), \/-(("a", Cl1(2, "bbaw"))))
  }

}