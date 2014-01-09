package org.obl.raz

import org.junit._

import PathFs._

class AppendTest {

  @Test
  def test1 = {
      val u01 = Raz.path("b").path(pathVar[Int]).param("a", "a")
     val u1 = Raz.path("a").append(u01)
     val u2 = Raz.path("a").path("b").path(pathVar[Int]).param("a", "a")
     val u3 = Raz.path("a").append(Raz.path("b").path(pathVar[Int]).param(paramValueVar[Int]("ccc")))
    
     println(u1.toF.apply(1).render)
     println(u2.toF.apply(1).render)
     println(u2.toF.apply(1).render)
     println(u3.toF.apply(13, 767).render)
     
     Assert.assertEquals("/a/b/1?a=a", u1.toF.apply(1).render)
     Assert.assertEquals("/a/b/1?a=a", u2.toF.apply(1).render)
     Assert.assertEquals("/a/b/1?a=a", u2.toF.apply(1).render)
     Assert.assertEquals("/a/b/13?ccc=767", u3.toF.apply(13, 767).render)
    
  }
}