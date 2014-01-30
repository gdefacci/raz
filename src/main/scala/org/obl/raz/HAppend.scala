package org.obl.raz

trait HAppend[-H <: HPath, -H1 <: HPath, +Out <: HPath] {
  def concat(h: H, h1: H1): Out
}

object HAppend {
  
  def apply[H1 <: HPath, H2 <: HPath, Out <: HPath](f:(H1, H2) => Out) = new HAppend[H1, H2, Out] {
    def concat(h1: H1, h2: H2): Out = f(h1,h2)
  }
  
  /** HPathNil ++ HPathNil */
  
  implicit def hPathNil_HPathNil1[R <: RelativePathAspect, A <: CanAddAspect, P1 <: CanHavePathAsPrefix] =
    HAppend[HPathNil[R, CanAddPath, CanHavePathAsPrefix], HPathNil[IsRelativePath, A, P1], HPathNil[R, A, CanHavePathAsPrefix]] { (h, h1) => 
      HPathNil[R, A, CanHavePathAsPrefix](h.path ++ h1.path)
  }
  
  implicit def hPathNil_HPathNil2[R <: RelativePathAspect, P <: CanHavePrefixAspect, P1 <: CanHaveParamsAsPrefix] =
    HAppend[HPathNil[R, CanAddParam, P], HPathNil[IsRelativePath, CanAddParam, P1], HPathNil[R, CanAddParam, P]] { (h,h1) => 
      HPathNil[R, CanAddParam, P](h.path ++ h1.path)
    }

    /** HPathCons ++ HPathNil */
  
  implicit def hPathCons_HPathNil1[R <: RelativePathAspect, H1 <: HPath, A <: CanAddAspect, P1 <: CanHavePathAsPrefix, T] =
    HAppend[HPathCons[H1, R, CanAddPath, CanHavePathAsPrefix, T], HPathNil[IsRelativePath, A, P1],  HPathCons[H1, R, A, CanHavePathAsPrefix, T]] { (h,h1) =>
      HPathConsFactory[R, A, CanHavePathAsPrefix].create(h.head, h.value.merge(h1.path))
    }
  
  implicit def hPathCons_HPathNil2[R <: RelativePathAspect, H1 <: HPath, A <: CanAddAspect, P <: CanHavePrefixAspect, T] =
    HAppend[HPathCons[H1, R, CanAddParam, P, T], HPathNil[IsRelativePath, A, CanHaveParamsAsPrefix],  HPathCons[H1, R, A, P, T]] { (h,h1) =>
        HPathConsFactory[R, A, P].create(h.head, h.value.merge(h1.path))
    }

  /** HPathNil ++ HPathCons*/

  implicit def hPathNil_HPathCons1[R <: RelativePathAspect, A <: CanAddAspect, P1 <: CanHavePathAsPrefix, H2 <: HPath, T, Out <: HPath](implicit preph: HAppend[HPathNil[R, CanAddPath, CanHavePathAsPrefix], H2, Out]) = {
    HAppend[HPathNil[R, CanAddPath, CanHavePathAsPrefix], HPathCons[H2, IsRelativePath, A, P1, T], HPathCons[Out, R, A, CanHavePathAsPrefix, T]] { (h1, h2) =>
      HPathConsFactory[R, A, CanHavePathAsPrefix].create(preph.concat(h1, h2.head), h2.value)
    }
  }
  
  implicit def hPathNil_HPathCons2[R <: RelativePathAspect, A <: CanAddParam, P <: CanHavePrefixAspect, H2 <: HPath, T, Out <: HPath](implicit preph: HAppend[HPathNil[R, CanAddParam, P], H2, Out]) = {
    HAppend[HPathNil[R, CanAddParam, P], HPathCons[H2, IsRelativePath, A, CanHaveParamsAsPrefix, T], HPathCons[Out, R, A, P, T]] { (h1, h2) =>
      HPathConsFactory[R, A, P].create(preph.concat(h1, h2.head), h2.value)
    }
  }

  /** HPathCons ++ HPathCons */

  implicit def hPathCons_HPathCons1[R <: RelativePathAspect, A2 <: CanAddAspect, P1 <: CanHavePathAsPrefix, H1 <: HPath, H2 <: HPath, T1, T2, Out <: HPath](implicit preph: HAppend[HPathCons[H1,R, CanAddPath, CanHavePathAsPrefix,T1], H2, Out]) = {
    HAppend[HPathCons[H1,R, CanAddPath, CanHavePathAsPrefix,T1], HPathCons[H2, IsRelativePath, A2, P1, T2], HPathCons[Out, R, A2, CanHavePathAsPrefix, T2]] { (h1, h2) =>
      HPathConsFactory[R, A2, CanHavePathAsPrefix].create(preph.concat(h1, h2.head), h2.value)
    }
  }
  
  implicit def hPathCons_HPathCons2[R <: RelativePathAspect, A2 <: CanAddParam, P1 <: CanHavePathAsPrefix,  H1 <: HPath, H2 <: HPath, T1, T2, Out <: HPath](implicit preph: HAppend[HPathCons[H1,R, CanAddParam, P1,T1], H2, Out]) = {
    HAppend[HPathCons[H1,R, CanAddParam, P1,T1], HPathCons[H2, IsRelativePath, A2, CanHaveParamsAsPrefix, T2], HPathCons[Out, R, A2, P1, T2]] { (h1, h2) =>
      HPathConsFactory[R, A2, P1].create(preph.concat(h1, h2.head), h2.value)
    }
  }
  
}