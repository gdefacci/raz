package org.obl.raz

import org.junit.Assert

object TestHelper {

  def check[H <: HPath,R <: RelativePathAspect,A <: CanAddAspect,P <: CanHavePrefixAspect, PTH <: Path, T, TUP](h:HPathCons[H,R,A,P,T], v:TUP)
  	(implicit hf:HPathF[HPathCons[H,R,A,P,T], TUP, PTH], matcher:PathMatcher[HPathCons[H,R,A,P,T],TUP]) = {
    
    h.matchPath(h.apply(v)) match {
      case Some(PathMatchResult(v1, rest)) => {
        Assert.assertEquals(v,v1)
      }
      case x => Assert.fail(x.toString)
    }
  }

  
}