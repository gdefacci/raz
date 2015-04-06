package org.obl.raz

trait EncHPathF[-H, -T, +P <: Path] {

  def apply(h:H):T => P
  
}

object EncHPathF {
  
  import  EncHPaths._
  
  private def toBasePath[P <: PathPosition, S <: P](pth:Path) =
    BasePath[P,S](Path.baseOf(pth), pth.path, pth.params, pth.fragment)
  
  def apply[H,T,PTH <: Path](f:H => T => PTH) =  new EncHPathF[H, T,PTH] {
    def apply(h:H):T => PTH = f(h)
  }
  
//  implicit def toEncPath[P <: PathPosition, S <: P] = apply[BasePath[P,S], Path, BasePath[P,S]] { p1 => p => p1 }
  
  private def sum[P <: PathPosition, S <: P](pths:Path*):BasePath[P,S] = {
    toBasePath[P,S](PathUtils.mergeAll(pths))
  }
  
  implicit def toHPathF1[P <: PathPosition, S <: P,T1] =
    apply[EncHPath1[P,S,T1], T1,BasePath[P,S]](h => t1 => sum[P,S](h.head.path,  h.value.apply(t1)))  
  

  implicit def toEncHPathF2[P <: PathPosition, S <: P,T1,T2] = 
    apply[EncHPath2[P,S,T1,T2], (T1,T2),BasePath[P,S]](h => t1 => sum[P,S](h.head.head.path,  h.head.value.apply(t1._1), h.value.apply(t1._2)) )  
  

  implicit def toEncHPathF3[P <: PathPosition, S <: P,T1,T2,T3] =
    apply[EncHPath3[P,S,T1,T2,T3], (T1,T2,T3),BasePath[P,S]](h => t1 => sum[P,S](h.head.head.head.path,  h.head.head.value.apply(t1._1), h.head.value.apply(t1._2), h.value.apply(t1._3)))  
  

  implicit def toEncHPathF4[P <: PathPosition, S <: P,T1,T2,T3,T4] =
    apply[EncHPath4[P,S,T1,T2,T3,T4], (T1,T2,T3,T4),BasePath[P,S]](h => t1 => sum[P,S](h.head.head.head.head.path,  h.head.head.head.value.apply(t1._1), h.head.head.value.apply(t1._2), h.head.value.apply(t1._3), h.value.apply(t1._4)))  
  

  implicit def toEncHPathF5[P <: PathPosition, S <: P,T1,T2,T3,T4,T5] =
    apply[EncHPath5[P,S,T1,T2,T3,T4,T5], (T1,T2,T3,T4,T5),BasePath[P,S]](h => t1 => sum[P,S](h.head.head.head.head.head.path,  h.head.head.head.head.value.apply(t1._1), h.head.head.head.value.apply(t1._2), h.head.head.value.apply(t1._3), h.head.value.apply(t1._4), h.value.apply(t1._5)))  
  

  implicit def toEncHPathF6[P <: PathPosition, S <: P,T1,T2,T3,T4,T5,T6] =
    apply[EncHPath6[P,S,T1,T2,T3,T4,T5,T6], (T1,T2,T3,T4,T5,T6),BasePath[P,S]](h => t1 => sum[P,S](h.head.head.head.head.head.head.path,  h.head.head.head.head.head.value.apply(t1._1), h.head.head.head.head.value.apply(t1._2), h.head.head.head.value.apply(t1._3), h.head.head.value.apply(t1._4), h.head.value.apply(t1._5), h.value.apply(t1._6)))  
  

  implicit def toEncHPathF7[P <: PathPosition, S <: P,T1,T2,T3,T4,T5,T6,T7] =
    apply[EncHPath7[P,S,T1,T2,T3,T4,T5,T6,T7], (T1,T2,T3,T4,T5,T6,T7),BasePath[P,S]](h => t1 => sum[P,S](h.head.head.head.head.head.head.head.path,  h.head.head.head.head.head.head.value.apply(t1._1), h.head.head.head.head.head.value.apply(t1._2), h.head.head.head.head.value.apply(t1._3), h.head.head.head.value.apply(t1._4), h.head.head.value.apply(t1._5), h.head.value.apply(t1._6), h.value.apply(t1._7)))  
  

  implicit def toEncHPathF8[P <: PathPosition, S <: P,T1,T2,T3,T4,T5,T6,T7,T8] =
    apply[EncHPath8[P,S,T1,T2,T3,T4,T5,T6,T7,T8], (T1,T2,T3,T4,T5,T6,T7,T8),BasePath[P,S]](h => t1 => sum[P,S](h.head.head.head.head.head.head.head.head.path,  h.head.head.head.head.head.head.head.value.apply(t1._1), h.head.head.head.head.head.head.value.apply(t1._2), h.head.head.head.head.head.value.apply(t1._3), h.head.head.head.head.value.apply(t1._4), h.head.head.head.value.apply(t1._5), h.head.head.value.apply(t1._6), h.head.value.apply(t1._7), h.value.apply(t1._8)))  
  

  implicit def toEncHPathF9[P <: PathPosition, S <: P,T1,T2,T3,T4,T5,T6,T7,T8,T9] =
    apply[EncHPath9[P,S,T1,T2,T3,T4,T5,T6,T7,T8,T9], (T1,T2,T3,T4,T5,T6,T7,T8,T9),BasePath[P,S]](h => t1 => sum[P,S](h.head.head.head.head.head.head.head.head.head.path,  h.head.head.head.head.head.head.head.head.value.apply(t1._1), h.head.head.head.head.head.head.head.value.apply(t1._2), h.head.head.head.head.head.head.value.apply(t1._3), h.head.head.head.head.head.value.apply(t1._4), h.head.head.head.head.value.apply(t1._5), h.head.head.head.value.apply(t1._6), h.head.head.value.apply(t1._7), h.head.value.apply(t1._8), h.value.apply(t1._9)))  
  

  implicit def toEncHPathF10[P <: PathPosition, S <: P,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10] =
    apply[EncHPath10[P,S,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10),BasePath[P,S]](h => t1 => sum[P,S](h.head.head.head.head.head.head.head.head.head.head.path,  h.head.head.head.head.head.head.head.head.head.value.apply(t1._1), h.head.head.head.head.head.head.head.head.value.apply(t1._2), h.head.head.head.head.head.head.head.value.apply(t1._3), h.head.head.head.head.head.head.value.apply(t1._4), h.head.head.head.head.head.value.apply(t1._5), h.head.head.head.head.value.apply(t1._6), h.head.head.head.value.apply(t1._7), h.head.head.value.apply(t1._8), h.head.value.apply(t1._9), h.value.apply(t1._10)))  
  

  implicit def toEncHPathF11[P <: PathPosition, S <: P,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11] =
    apply[EncHPath11[P,S,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11),BasePath[P,S]](h => t1 => sum[P,S](h.head.head.head.head.head.head.head.head.head.head.head.path,  h.head.head.head.head.head.head.head.head.head.head.value.apply(t1._1), h.head.head.head.head.head.head.head.head.head.value.apply(t1._2), h.head.head.head.head.head.head.head.head.value.apply(t1._3), h.head.head.head.head.head.head.head.value.apply(t1._4), h.head.head.head.head.head.head.value.apply(t1._5), h.head.head.head.head.head.value.apply(t1._6), h.head.head.head.head.value.apply(t1._7), h.head.head.head.value.apply(t1._8), h.head.head.value.apply(t1._9), h.head.value.apply(t1._10), h.value.apply(t1._11)))  
  

  implicit def toEncHPathF12[P <: PathPosition, S <: P,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12] =
    apply[EncHPath12[P,S,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12),BasePath[P,S]](h => t1 => sum[P,S](h.head.head.head.head.head.head.head.head.head.head.head.head.path,  h.head.head.head.head.head.head.head.head.head.head.head.value.apply(t1._1), h.head.head.head.head.head.head.head.head.head.head.value.apply(t1._2), h.head.head.head.head.head.head.head.head.head.value.apply(t1._3), h.head.head.head.head.head.head.head.head.value.apply(t1._4), h.head.head.head.head.head.head.head.value.apply(t1._5), h.head.head.head.head.head.head.value.apply(t1._6), h.head.head.head.head.head.value.apply(t1._7), h.head.head.head.head.value.apply(t1._8), h.head.head.head.value.apply(t1._9), h.head.head.value.apply(t1._10), h.head.value.apply(t1._11), h.value.apply(t1._12)))  
  

  implicit def toEncHPathF13[P <: PathPosition, S <: P,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13] =
    apply[EncHPath13[P,S,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13),BasePath[P,S]](h => t1 => sum[P,S](h.head.head.head.head.head.head.head.head.head.head.head.head.head.path,  h.head.head.head.head.head.head.head.head.head.head.head.head.value.apply(t1._1), h.head.head.head.head.head.head.head.head.head.head.head.value.apply(t1._2), h.head.head.head.head.head.head.head.head.head.head.value.apply(t1._3), h.head.head.head.head.head.head.head.head.head.value.apply(t1._4), h.head.head.head.head.head.head.head.head.value.apply(t1._5), h.head.head.head.head.head.head.head.value.apply(t1._6), h.head.head.head.head.head.head.value.apply(t1._7), h.head.head.head.head.head.value.apply(t1._8), h.head.head.head.head.value.apply(t1._9), h.head.head.head.value.apply(t1._10), h.head.head.value.apply(t1._11), h.head.value.apply(t1._12), h.value.apply(t1._13)))  
  

  implicit def toEncHPathF14[P <: PathPosition, S <: P,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14] =
    apply[EncHPath14[P,S,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14),BasePath[P,S]](h => t1 => sum[P,S](h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.path,  h.head.head.head.head.head.head.head.head.head.head.head.head.head.value.apply(t1._1), h.head.head.head.head.head.head.head.head.head.head.head.head.value.apply(t1._2), h.head.head.head.head.head.head.head.head.head.head.head.value.apply(t1._3), h.head.head.head.head.head.head.head.head.head.head.value.apply(t1._4), h.head.head.head.head.head.head.head.head.head.value.apply(t1._5), h.head.head.head.head.head.head.head.head.value.apply(t1._6), h.head.head.head.head.head.head.head.value.apply(t1._7), h.head.head.head.head.head.head.value.apply(t1._8), h.head.head.head.head.head.value.apply(t1._9), h.head.head.head.head.value.apply(t1._10), h.head.head.head.value.apply(t1._11), h.head.head.value.apply(t1._12), h.head.value.apply(t1._13), h.value.apply(t1._14)))  
  

  implicit def toEncHPathF15[P <: PathPosition, S <: P,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15] =
    apply[EncHPath15[P,S,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15),BasePath[P,S]](h => t1 => sum[P,S](h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.path,  h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value.apply(t1._1), h.head.head.head.head.head.head.head.head.head.head.head.head.head.value.apply(t1._2), h.head.head.head.head.head.head.head.head.head.head.head.head.value.apply(t1._3), h.head.head.head.head.head.head.head.head.head.head.head.value.apply(t1._4), h.head.head.head.head.head.head.head.head.head.head.value.apply(t1._5), h.head.head.head.head.head.head.head.head.head.value.apply(t1._6), h.head.head.head.head.head.head.head.head.value.apply(t1._7), h.head.head.head.head.head.head.head.value.apply(t1._8), h.head.head.head.head.head.head.value.apply(t1._9), h.head.head.head.head.head.value.apply(t1._10), h.head.head.head.head.value.apply(t1._11), h.head.head.head.value.apply(t1._12), h.head.head.value.apply(t1._13), h.head.value.apply(t1._14), h.value.apply(t1._15)))  
  

  implicit def toEncHPathF16[P <: PathPosition, S <: P,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16] =
    apply[EncHPath16[P,S,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16),BasePath[P,S]](h => t1 => sum[P,S](h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.path,  h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value.apply(t1._1), h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value.apply(t1._2), h.head.head.head.head.head.head.head.head.head.head.head.head.head.value.apply(t1._3), h.head.head.head.head.head.head.head.head.head.head.head.head.value.apply(t1._4), h.head.head.head.head.head.head.head.head.head.head.head.value.apply(t1._5), h.head.head.head.head.head.head.head.head.head.head.value.apply(t1._6), h.head.head.head.head.head.head.head.head.head.value.apply(t1._7), h.head.head.head.head.head.head.head.head.value.apply(t1._8), h.head.head.head.head.head.head.head.value.apply(t1._9), h.head.head.head.head.head.head.value.apply(t1._10), h.head.head.head.head.head.value.apply(t1._11), h.head.head.head.head.value.apply(t1._12), h.head.head.head.value.apply(t1._13), h.head.head.value.apply(t1._14), h.head.value.apply(t1._15), h.value.apply(t1._16)))  
  

  implicit def toEncHPathF17[P <: PathPosition, S <: P,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17] =
    apply[EncHPath17[P,S,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17),BasePath[P,S]](h => t1 => sum[P,S](h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.path,  h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value.apply(t1._1), h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value.apply(t1._2), h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value.apply(t1._3), h.head.head.head.head.head.head.head.head.head.head.head.head.head.value.apply(t1._4), h.head.head.head.head.head.head.head.head.head.head.head.head.value.apply(t1._5), h.head.head.head.head.head.head.head.head.head.head.head.value.apply(t1._6), h.head.head.head.head.head.head.head.head.head.head.value.apply(t1._7), h.head.head.head.head.head.head.head.head.head.value.apply(t1._8), h.head.head.head.head.head.head.head.head.value.apply(t1._9), h.head.head.head.head.head.head.head.value.apply(t1._10), h.head.head.head.head.head.head.value.apply(t1._11), h.head.head.head.head.head.value.apply(t1._12), h.head.head.head.head.value.apply(t1._13), h.head.head.head.value.apply(t1._14), h.head.head.value.apply(t1._15), h.head.value.apply(t1._16), h.value.apply(t1._17)))  
  

  implicit def toEncHPathF18[P <: PathPosition, S <: P,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18] =
    apply[EncHPath18[P,S,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18),BasePath[P,S]](h => t1 => sum[P,S](h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.path,  h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value.apply(t1._1), h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value.apply(t1._2), h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value.apply(t1._3), h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value.apply(t1._4), h.head.head.head.head.head.head.head.head.head.head.head.head.head.value.apply(t1._5), h.head.head.head.head.head.head.head.head.head.head.head.head.value.apply(t1._6), h.head.head.head.head.head.head.head.head.head.head.head.value.apply(t1._7), h.head.head.head.head.head.head.head.head.head.head.value.apply(t1._8), h.head.head.head.head.head.head.head.head.head.value.apply(t1._9), h.head.head.head.head.head.head.head.head.value.apply(t1._10), h.head.head.head.head.head.head.head.value.apply(t1._11), h.head.head.head.head.head.head.value.apply(t1._12), h.head.head.head.head.head.value.apply(t1._13), h.head.head.head.head.value.apply(t1._14), h.head.head.head.value.apply(t1._15), h.head.head.value.apply(t1._16), h.head.value.apply(t1._17), h.value.apply(t1._18)))  
  

  implicit def toEncHPathF19[P <: PathPosition, S <: P,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19] =
    apply[EncHPath19[P,S,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19),BasePath[P,S]](h => t1 => sum[P,S](h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.path,  h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value.apply(t1._1), h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value.apply(t1._2), h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value.apply(t1._3), h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value.apply(t1._4), h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value.apply(t1._5), h.head.head.head.head.head.head.head.head.head.head.head.head.head.value.apply(t1._6), h.head.head.head.head.head.head.head.head.head.head.head.head.value.apply(t1._7), h.head.head.head.head.head.head.head.head.head.head.head.value.apply(t1._8), h.head.head.head.head.head.head.head.head.head.head.value.apply(t1._9), h.head.head.head.head.head.head.head.head.head.value.apply(t1._10), h.head.head.head.head.head.head.head.head.value.apply(t1._11), h.head.head.head.head.head.head.head.value.apply(t1._12), h.head.head.head.head.head.head.value.apply(t1._13), h.head.head.head.head.head.value.apply(t1._14), h.head.head.head.head.value.apply(t1._15), h.head.head.head.value.apply(t1._16), h.head.head.value.apply(t1._17), h.head.value.apply(t1._18), h.value.apply(t1._19)))  
  

  implicit def toEncHPathF20[P <: PathPosition, S <: P,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20] =
    apply[EncHPath20[P,S,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20),BasePath[P,S]](h => t1 => sum[P,S](h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.path,  h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value.apply(t1._1), h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value.apply(t1._2), h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value.apply(t1._3), h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value.apply(t1._4), h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value.apply(t1._5), h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value.apply(t1._6), h.head.head.head.head.head.head.head.head.head.head.head.head.head.value.apply(t1._7), h.head.head.head.head.head.head.head.head.head.head.head.head.value.apply(t1._8), h.head.head.head.head.head.head.head.head.head.head.head.value.apply(t1._9), h.head.head.head.head.head.head.head.head.head.head.value.apply(t1._10), h.head.head.head.head.head.head.head.head.head.value.apply(t1._11), h.head.head.head.head.head.head.head.head.value.apply(t1._12), h.head.head.head.head.head.head.head.value.apply(t1._13), h.head.head.head.head.head.head.value.apply(t1._14), h.head.head.head.head.head.value.apply(t1._15), h.head.head.head.head.value.apply(t1._16), h.head.head.head.value.apply(t1._17), h.head.head.value.apply(t1._18), h.head.value.apply(t1._19), h.value.apply(t1._20)))  
  

  implicit def toEncHPathF21[P <: PathPosition, S <: P,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21] =
    apply[EncHPath21[P,S,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21),BasePath[P,S]](h => t1 => sum[P,S](h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.path,  h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value.apply(t1._1), h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value.apply(t1._2), h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value.apply(t1._3), h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value.apply(t1._4), h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value.apply(t1._5), h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value.apply(t1._6), h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value.apply(t1._7), h.head.head.head.head.head.head.head.head.head.head.head.head.head.value.apply(t1._8), h.head.head.head.head.head.head.head.head.head.head.head.head.value.apply(t1._9), h.head.head.head.head.head.head.head.head.head.head.head.value.apply(t1._10), h.head.head.head.head.head.head.head.head.head.head.value.apply(t1._11), h.head.head.head.head.head.head.head.head.head.value.apply(t1._12), h.head.head.head.head.head.head.head.head.value.apply(t1._13), h.head.head.head.head.head.head.head.value.apply(t1._14), h.head.head.head.head.head.head.value.apply(t1._15), h.head.head.head.head.head.value.apply(t1._16), h.head.head.head.head.value.apply(t1._17), h.head.head.head.value.apply(t1._18), h.head.head.value.apply(t1._19), h.head.value.apply(t1._20), h.value.apply(t1._21)))  
  

  implicit def toEncHPathF22[P <: PathPosition, S <: P,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21,T22] =
    apply[EncHPath22[P,S,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21,T22], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21,T22),BasePath[P,S]](h => t1 => sum[P,S](h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.path,  h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value.apply(t1._1), h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value.apply(t1._2), h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value.apply(t1._3), h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value.apply(t1._4), h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value.apply(t1._5), h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value.apply(t1._6), h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value.apply(t1._7), h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value.apply(t1._8), h.head.head.head.head.head.head.head.head.head.head.head.head.head.value.apply(t1._9), h.head.head.head.head.head.head.head.head.head.head.head.head.value.apply(t1._10), h.head.head.head.head.head.head.head.head.head.head.head.value.apply(t1._11), h.head.head.head.head.head.head.head.head.head.head.value.apply(t1._12), h.head.head.head.head.head.head.head.head.head.value.apply(t1._13), h.head.head.head.head.head.head.head.head.value.apply(t1._14), h.head.head.head.head.head.head.head.value.apply(t1._15), h.head.head.head.head.head.head.value.apply(t1._16), h.head.head.head.head.head.value.apply(t1._17), h.head.head.head.head.value.apply(t1._18), h.head.head.head.value.apply(t1._19), h.head.head.value.apply(t1._20), h.head.value.apply(t1._21), h.value.apply(t1._22)))  

}