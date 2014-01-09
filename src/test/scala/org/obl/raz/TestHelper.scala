package org.obl.raz

import org.junit._

object TestHelper {
  
  import HElems._
  def check1[T1, H <: HElem1[T1]](u1:H, v1:T1)(implicit hf:HF[H,T1 => Path], pm:PathMatcher[H,T1], mp:Mapper[H]) {
    val f1 = u1.toF
    val m1 = u1.matcher
    
    m1(f1(v1)) match {
      case Some(PathMatchResult(p1, Path.empty )) => {
        Assert.assertEquals(v1,p1)
      }
      case x => Assert.fail(x.toString)
    } 
  }
  
  def check3[T1,T2,T3, H <: HElem3[T1,T2,T3]](u1:H, v1:T1, v2:T2, v3:T3)(implicit hf:HF[H,(T1,T2,T3) => Path], pm:PathMatcher[H,(T1,T2,T3)], mp:Mapper[H]) {
    val f1 = u1.toF
    val m1 = u1.matcher
    
    m1(f1(v1,v2,v3)) match {
      case Some(PathMatchResult((p1,p2,p3), Path.empty )) => {
        Assert.assertEquals(v1,p1)
        Assert.assertEquals(v2,p2)
        Assert.assertEquals(v3,p3)
      }
      case x => Assert.fail(x.toString)
    } 
  }

}