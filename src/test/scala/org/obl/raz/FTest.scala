package org.obl.raz

import org.junit._
import org.junit.Assert._

class FTest {

  import PathFs._
  
  @Test
  def test1 = {
    val u1 = Raz.add("a").add(pathVar[Int]).add("b").add(pathVar[String])
	
	val pth =  u1(10, "sd")

	assertEquals("/a/10/b/sd", pth.render)
  }
  
  @Test
  def test2 = {
    val qvar1 = paramValueVar[String]("pippo")
    
    val u1 = Raz.add("a").add(pathVar[Int]).add("b").addParam( qvar1 ).addParam("aa", "aa")
	
	val pth =  u1(10, "aaaa")

	assertEquals("/a/10/b?pippo=aaaa&aa=aa", pth.render)
  }
 
  @Test
  def test_# = {
     
	 val hp1 = (Raz / "a" ) ## "myfragment" 
     val hp2 = (Raz / "a" && ("par", "1")) ## "myfragment" 
     
     println(hp1.render)
     assertEquals("/a#myfragment", hp1.render)
     assertEquals("/a?par=1#myfragment", hp2.render)
     
     val u = Raz.add("a").add("b").add(pathVar[Int]).addParam(paramValueVar[Int]("ccc")).addFragment("myfrag")
     
     val hp3 = u(1,2)
     
     assertEquals("/a/b/1?ccc=2#myfrag", hp3.render)
 
     val utp1 = Raz.add("a").add("b").add(pathVar[Int]).add(pathVar[Int]).addFragment("f").toUriTemplate("a","b")
     
     assertEquals("/a/b/{a}/{b}#f", utp1.render)
     
     val utp2 = u.toUriTemplate("sym1", "sym2")
     
     assertEquals("/a/b/{sym1}?ccc={sym2}#myfrag", utp2.render)
   }
  
}