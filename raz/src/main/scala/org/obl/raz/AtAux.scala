package org.obl.raz

trait AtAux[H, OUT] {

  def apply(h: H): PathBase => OUT

}

object AtAux {

  def apply[H, OUT](f: H => PathBase => OUT) =
    new AtAux[H, OUT] {
      def apply(h: H) = f(h)
    }
  
  implicit def basePathAt[P <: SegmentPosition, S <: P] = apply[BasePath[P,S], BasePath[BasePosition, S]] { hp => base =>
    BasePath[BasePosition, S](Some(base), hp.path, hp.params, hp.fragment)
  }

  implicit def hpathNilAt[P <: SegmentPosition, S <: P] = apply[HPathNil[P,S], HPathNil[BasePosition, S]] { hp => base =>
    HPathNil[BasePosition, S](basePathAt[P,S](hp.path)(base))
  }
  
  implicit def hpathConsAt[H <: HPath, H1 <: HPath, TD, TE,UT, P <: SegmentPosition, S <: P](implicit headAt:AtAux[H, H1]) = 
    apply[HPathCons[H,P, S, TD, TE, UT], HPathCons[H1,BasePosition, S, TD, TE, UT]] { hp => base =>
      HPathCons[H1, BasePosition, S, TD, TE, UT](headAt.apply(hp.head)(base), hp.value )
    }
  
}