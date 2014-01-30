package org.obl.raz

import org.junit._

import PathFs._

class AppendTest {

  @Test
  def test1 = {
     val u01 = Raz.add("b").add(pathVar[Int]).addParam("a", "a")
     val u1 = Raz.add("a").append(u01)
     val u2 = Raz.add("a").add("b").add(pathVar[Int]).addParam("a", "a")
     val u3 = Raz.add("a").append(Raz.add("b").add(pathVar[Int]).addParam(paramValueVar[Int]("ccc")))
    
     println(u1(1).render)
     println(u2(1).render)
     println(u2(1).render)
     println(u3(13, 767).render)
     
     Assert.assertEquals("/a/b/1?a=a", u1(1).render)
     Assert.assertEquals("/a/b/1?a=a", u2(1).render)
     Assert.assertEquals("/a/b/1?a=a", u2(1).render)
     Assert.assertEquals("/a/b/13?ccc=767", u3(13, 767).render)
    
  }
}