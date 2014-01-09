package org.obl.raz

import org.junit._
import org.junit.Assert._


class MatcherTest {

  import PathFs._
  
  @Test
  def testMatcher1 = {

	val u2 = Raz.path("a").path("b").path(pathVar[Int]).path("c").param(paramValueVar[String]("aaa"))
	
	val mtchr = u2.matcher.apply(u2.toF.apply(10, "bb"))
	
	mtchr match {
	  case Some(PathMatchResult((10,"bb"),Path.empty)) => ()
	  case x => fail("matcher " + mtchr)
	}
  }


  @Test
  def testMatcher2 = {

	val u2 = Raz.path("a").path("b").path(pathVar[Int]).path("c").param(paramValueVar[String]("aaa"))
	val u3 = u2.param("d", "eeee")
	
	val mtchr1 = u2.matcher.apply(u3.toF.apply(11, "bubu"))
	
	mtchr1 match {
	  case Some(PathMatchResult((11,"bubu"),RootUri(PathSg.empty,Seq(QParamSg("d",Some("eeee")))))) => ()
	  case x => fail("matcher " + mtchr1)
	}
	
	mtchr1 match {
	  case Some(PathMatchResult(res,RootUri(PathSg.empty,Seq(QParamSg("d",Some("eeee")))))) => {
		  assert( res._1 == 11)
	  }
	  case x => fail("matcher " + mtchr1)
	}
  }
  
  case class Cl1(a:Int, b:String)
  
  @Test
  def testMapAndMAtch = {
    val sfx = (Raz && paramValueVar[Int]("a") && paramValueVar[String]("b")).mapper.mapTo(Converter(Cl1.tupled, Cl1.unapply))
    val prfx = Raz / "h" / pathVar[String]
    
    val u1 = prfx.append(sfx)
    
    println( u1.matcher.apply(u1.toF.apply("a", Cl1(2, "bbaw"))) )
  }

  @Test
  def optTest = {
    
    val u = Raz.path("a").path("b").path(pathVar[String]).param(optParamVar[Int]("opt"))
    
    val u1 = u.toF.apply("bbbb", Some(3))
    println( u.toF.apply("bbb", Some(3)).render )
    println( u.matcher.apply(u1) )
    
    Assert.assertEquals("bbbb" -> Some(3), u.matcher.apply(u1).get.value)
  }
   
  @Test
   def noneTest = {

    val u = Raz.path("a").path("b").path(pathVar[String]).param(optParamVar[Int]("opt"))
    val u1 = u.toF.apply("bbbb", None)
 
    Assert.assertEquals("bbbb" -> None, u.matcher.apply(u1).get.value)
    Assert.assertTrue(u.matcher.apply(u1).get.rest.isEmpty)
    
    Assert.assertEquals("c" -> None, u.matcher.apply(Path(PathSg(Seq("a","b","c")), Seq(QParamSg("opt", Some(""))))).get.value)
    Assert.assertTrue(u.matcher.apply(Path(PathSg(Seq("a","b","c")), Seq(QParamSg("opt", Some(""))))).get.rest.isEmpty)
  }
  

}