package org.obl.raz

import org.junit.Assert
import scalaz.{-\/, \/-}

object TestHelper {

  import EncHPathF._
  
//  def check[H <: HPath,R <: RelativePathAspect,A <: CanAddAspect,P <: CanHavePrefixAspect, PTH <: Path, E,D, TUP,UT](h:HPathCons[H,R,A,P,E,D,UT], v:TUP)
//  	(implicit hf:EncHPathF[HPathCons[H,R,A,P,E,D,UT], TUP, PTH], matcher:PathMatcher[HPathCons[H,R,A,P,E,D,UT],TUP]) = {
//    
//    h.decodeFull(h.apply(v)) match {
//      case \/-(v1) => Assert.assertEquals(v,v1)
//      case x => Assert.fail(x.toString)
//    }
//  }
  
  def check[A <: CanAddAspect,P <: CanHavePrefixAspect, T](h:PathConverter[T,T,_,A,P], v:T) = {
    h.decodeFull(h.encode(v)) match {
      case \/-(v1) => Assert.assertEquals(v,v1)
      case x => Assert.fail(x.toString)
    }
  }

  
}