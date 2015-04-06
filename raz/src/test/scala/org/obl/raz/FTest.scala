package org.obl.raz

import org.junit._
import org.junit.Assert._

class FTest {

  import PathConverter._
  
  @Test
  def test1 = {
//		  val u1 = RelativePath.add("a").add(Segment.int).add("b").add(Segment.string)
    val u1 = RelativePath.add("a").add(Segment.int).add("b").add(Segment.string)
	
	val pth =  u1(10, "sd")

	assertEquals("/a/10/b/sd", pth.render)
  }
  
  @Test
  def test2 = {
//    val qvar1 = Param.string("pippo")
    		val qvar1 = Param.string("pippo")
    
    val u1 = RelativePath.add("a").add(Segment.int).add("b").addParam( qvar1 ).addParam("aa", "aa")
	
	val pth =  u1(10, "aaaa")

	assertEquals("/a/10/b?pippo=aaaa&aa=aa", pth.render)
  }
 
  @Test
  def test0_#= {
    val utp1 = RelativePath.add("a").add("b").add(Segment.int).add(Segment.int).addFragment("f").toUriTemplate("a","b").render
    assertEquals("/a/b/{a}/{b}#f", utp1)
    
  }
  
  @Test
  def test_# = {
     
	   val hp1 = RelativePath / "a" &# "myfragment" 
     val hp2 = RelativePath / "a" && ("par", "1") &# "myfragment" 
     
     println(hp1.render)
     assertEquals("/a#myfragment", hp1.render)
     assertEquals("/a?par=1#myfragment", hp2.render)
     
//     val u = RelativePath.add("a").add("b").add(Segment.int).addParam(Param.int("ccc")).addFragment("myfrag")
     val u = RelativePath.add("a").add("b").add(Segment.int).addParam(Param.int("ccc")).addFragment("myfrag")
     
     val u1 = RelativePath.add("a").add("ab ba").add(Segment.int).addParam(Param.int("ccc")).addFragment("myfrag")
     
     val hp3 = u(1,2)
     val hp4 = u1(1,2)
     
     assertEquals("ab%20ba", UriPartEncode.encode(UriPartEncode.pathUnescaped)("ab ba"))
     
     assertEquals("/a/b/1?ccc=2#myfrag", hp3.render)
     assertEquals("/a/ab%20ba/1?ccc=2#myfrag", hp4.render)
 
     val utp1 =  RelativePath.add("a").add("b").add(Segment.int).add(Segment.int).addFragment("f").toUriTemplate("a","b").render
     val utp1a = RelativePath.add("a").add("ab ba").add(Segment.int).add(Segment.int).addFragment("f").toUriTemplate("a","b").render
     
     assertEquals("/a/b/{a}/{b}#f", utp1)
     assertEquals("/a/ab%20ba/{a}/{b}#f", utp1a)
     
     val utp2 = u.toUriTemplate("sym1", "sym2").render
     val utp2a = u1.toUriTemplate("sym1", "sym2").render
     
     assertEquals("/a/b/{sym1}?ccc={sym2}#myfrag", utp2)
     assertEquals("/a/ab%20ba/{sym1}?ccc={sym2}#myfrag", utp2a)
   }
  
  @Test
  def testUT1 = {
   val state = RelativePath / "countries" / Segment.string / "states" / Segment.string 

    assert("/countries/{it}/states/{mi}" == state.toUriTemplate("it", "mi").render)
    		
   println(state.toUriTemplate("country", "state"))
    
  }
  
  @Test
  def testSgList = {
    val u1 = RelativePath / "base" / seq(PathConverter(RelativePath / "property" / Segment.string))
    println(u1(List("p1", "p2", "p3")))
    assertEquals("/base/property/p1/property/p2/property/p3", u1(List("p1", "p2", "p3")).render )
    
    TestHelper.check(u1, Seq("p1","p3", "p4"))
  }
  
}