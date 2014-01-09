package org.obl.raz

import org.junit._
import org.junit.Assert._

class FTest {

  import PathFs._
  
  @Test
  def test1 = {
    val u1 = Raz.path("a").path(pathVar[Int]).path("b").path(pathVar[String])
	
	val pth =  u1.toF.apply(10, "sd")

	assertEquals("/a/10/b/sd", pth.render)
  }
  
  @Test
  def test2 = {
    val qvar1 = paramValueVar[String]("pippo")
    
    val u1 = Raz.path("a").path(pathVar[Int]).path("b").param( qvar1 ).param("aa", "aa")
	
	val pth =  u1.toF.apply(10, "aaaa")

	assertEquals("/a/10/b?pippo=aaaa&aa=aa", pth.render)
  }
  
}