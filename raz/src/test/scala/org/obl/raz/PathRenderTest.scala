package org.obl.raz

import org.scalatest.FunSuite
import org.scalatest.Matchers

class PathRenderTest extends FunSuite with Matchers {
  
  test("aaa") {

    println(HTTP("site.com").render)
    println(HTTP("site.com", 8020).render)
    println((HTTP("site.com", 8020) / "app").render)
    
    
  }
  
}