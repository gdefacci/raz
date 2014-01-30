package org.obl.raz

import org.junit._
import org.junit.Assert._


class MatcherTest {

  import PathFs._
  
  @Test
  def testMatcher1 = {

	val u2 = Raz.add("a").add("b").add(pathVar[Int]).add("c").addParam(paramValueVar[String]("aaa"))
	
	val mtchr = u2.matchPath(u2(10, "bb"))
	
	mtchr match {
	  case Some(PathMatchResult((10,"bb"),Path.empty)) => ()
	  case x => {
	    println(x)
	    fail("matcher " + mtchr)
	  }
	}
  }


  @Test
  def testMatcher2 = {

	val u2 = Raz.add("a").add("b").add(pathVar[Int]).add("c").addParam(paramValueVar[String]("aaa"))
	val u3 = u2.addParam("d", "eeee")
	
	val mtchr1 = u2.matchPath(u3(11, "bubu"))
	
	mtchr1 match {
	  case Some(PathMatchResult((11,"bubu"),Path(None, PathSg.empty,Seq(QParamSg("d",Some("eeee"))), None)) ) => ()
	  case x => fail("matcher " + mtchr1)
	}
	
	mtchr1 match {
	  case Some(PathMatchResult(res,Path(None, PathSg.empty,Seq(QParamSg("d",Some("eeee"))), None) )) => {
		  assert( res._1 == 11)
	  }
	  case x => fail("matcher " + mtchr1)
	}
  }
  
  case class Cl1(a:Int, b:String)
  
  @Test
  def testMapAndMAtch = {
    val sfx = (Raz && paramValueVar[Int]("a") && paramValueVar[String]("b")).mapTo(Converter(Cl1.tupled, Cl1.unapply))
    val prfx = Raz / "h" / pathVar[String]
    
    val u1 = prfx concat sfx
    println( u1.matchPath(u1("a", Cl1(2, "bbaw"))) )
  }

  @Test
  def optTest = {
    
    val u = Raz.add("a").add("b").add(pathVar[String]).addParam(optParamVar[Int]("opt"))
    
    val u1 = u("bbbb", Some(3))
    println( u("bbb", Some(3)).render )
    println( u.matchPath(u1) )
    
    Assert.assertEquals("bbbb" -> Some(3), u.matchPath(u1).get.value)
  }
   
  @Test
   def noneTest = {

    val u = Raz.add("a").add("b").add(pathVar[String]).addParam(optParamVar[Int]("opt"))
    val u1 = u("bbbb", None)
 
    Assert.assertEquals("bbbb" -> None, u.matchPath(u1).get.value)
    Assert.assertTrue(u.matchPath(u1).get.rest.isEmpty)
    
    Assert.assertEquals("c" -> None, u.matchPath(Path(None, PathSg(Seq("a","b","c")), Seq(QParamSg("opt", Some(""))), None )).get.value)
    Assert.assertTrue(u.matchPath(Path(None, PathSg(Seq("a","b","c")), Seq(QParamSg("opt", Some(""))), None)).get.rest.isEmpty)
  }
  

}