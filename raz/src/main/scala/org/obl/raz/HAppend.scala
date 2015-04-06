package org.obl.raz

trait HAppend[H <: HPath, H1 <: HPath, +Out <: HPath] {
  def concat(h: H, h1: H1): Out
}

object HAppend {
  
  def apply[H1 <: HPath, H2 <: HPath, Out <: HPath](f:(H1, H2) => Out) = new HAppend[H1, H2, Out] {
    def concat(h1: H1, h2: H2): Out = f(h1,h2)
  }
  
  implicit def hPathNil_HPathNil[P1 <: PathPosition, S1 <: P1, P2 <: S1, S2 <: P2] =
    HAppend[HPathNil[P1, S1], HPathNil[P2, S2], HPathNil[P1, S2]] { (h,h1) => 
      HPathNil[P1, S2](BasePath.sum(h.path, h1.path))
    }

  implicit def hPathCons_HPathNil[H1 <: HPath, P1 <: PathPosition, S1 <: P1, P2 <: S1, S2 <: P2, TD,TE,UT] =
    HAppend[HPathCons[H1, P1,S1, TD,TE,UT], HPathNil[P2, S2],  HPathCons[H1, P1, S2, TD,TE,UT]] { (h,h1) =>
        HPathConsFactory[P1].create(h.head, h.value.addPath(h1.path))
    }
  
  implicit def hPathNil_HPathCons[H2 <: HPath, P1 <: PathPosition, S1 <: P1, P2 <: S1, S2 <: P2, TD,TE,UT, Out <: HPath](implicit preph: HAppend[HPathNil[P1,S1], H2, Out]) = {
    HAppend[HPathNil[P1, S1], HPathCons[H2, P2, S2, TD,TE,UT], HPathCons[Out, P1, S2, TD,TE,UT]] { (h1, h2) =>
      HPathConsFactory[P1].create(preph.concat(h1, h2.head), h2.value)
    }
  }
  
  implicit def hPathCons_HPathCons[H1 <: HPath, H2 <: HPath, P1 <: PathPosition, S1 <: P1, P2 <: S1, S2 <: P2, TD1, TE1, UT1, TD2, TE2, UT2, Out <: HPath](implicit preph: HAppend[HPathCons[H1,P1,S1,TD1,TE1,UT1], H2, Out]) = {
    HAppend[HPathCons[H1,P1,S1,TD1, TE1, UT1], HPathCons[H2, P2,S2, TD2, TE2, UT2], HPathCons[Out, P1,S2, TD2, TE2, UT2]] { (h1, h2) =>
      HPathConsFactory[P1].create(preph.concat(h1, h2.head), h2.value)
    }
  }


   
}