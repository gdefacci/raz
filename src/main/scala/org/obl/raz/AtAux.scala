package org.obl.raz

trait AtAux[H <: HPath, OUT <: HPath] {

  def apply(h: H): PathBase => OUT

}

object AtAux {

  def apply[H <: HPath, OUT <: HPath](f: H => PathBase => OUT) =
    new AtAux[H, OUT] {
      def apply(h: H) = f(h)
    }

  implicit def hpathNilAt[A <: CanAddAspect, P <: CanHavePathAsPrefix] = apply[HPathNil[IsRelativePath, A, P], HPathNil[IsAbsolutePath, A, CanHavePrefixAspect]] { hp => base =>
    HPathNil[IsAbsolutePath, A, CanHavePrefixAspect](hp.path.at(base))
  }
  
  implicit def hpathConsAt[H <: HPath, H1 <: HPath, TD, TE,UT, A <: CanAddAspect, P <: CanHavePathAsPrefix](implicit headAt:AtAux[H, H1]) = 
    apply[HPathCons[H,IsRelativePath, A, P, TD, TE, UT], HPathCons[H1,IsAbsolutePath, A, CanHavePrefixAspect, TD, TE, UT]] { hp => base =>
      HPathCons[H1, IsAbsolutePath, A, CanHaveParamsAsPrefix, TD, TE, UT](headAt.apply(hp.head)(base), hp.value )
    }
  
}