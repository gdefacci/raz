package org.obl.raz

trait UTHPathF[-H <: HPath, -T] {

  def apply(h:H):T => UriTemplate
  
}

object UTHPathF {
  
  import  UTEncHPaths._
  
//  private def toBasePath[R <: RelativePathAspect,A <: CanAddAspect,P <: CanHavePrefixAspect](pth:Path) =
//    BasePath[R,A,P](pth.base, pth.path, pth.params, pth.fragment)
  
  def apply[H <: HPath,T](f:H => T => UriTemplate) =  new UTHPathF[H, T] {
    def apply(h:H):T => UriTemplate = f(h)
  }
  
  private def sum(pths:UriTemplate*) = {
    UriTemplateUtils.mergeAll(pths)
  }
  
  implicit def toUTHPattoUTHPathF1[R <: RelativePathAspect,A <: CanAddAspect,P <: CanHavePrefixAspect,T1] =
    apply[UTEncHPath1[R,A,P,T1], T1](h => t1 => sum(UriTemplate(h.head.path),  h.value.toUriTemplate(t1)))  
  

  implicit def toUTHPathF2[R <: RelativePathAspect,A <: CanAddAspect,P <: CanHavePrefixAspect,T1,T2] = 
    apply[UTEncHPath2[R,A,P,T1,T2], (T1,T2)](h => t1 => sum(UriTemplate(h.head.head.path),  h.head.value.toUriTemplate(t1._1), h.value.toUriTemplate(t1._2)) )  
  

  implicit def toUTHPathF3[R <: RelativePathAspect,A <: CanAddAspect,P <: CanHavePrefixAspect,T1,T2,T3] =
    apply[UTEncHPath3[R,A,P,T1,T2,T3], (T1,T2,T3)](h => t1 => sum(UriTemplate(h.head.head.head.path),  h.head.head.value.toUriTemplate(t1._1), h.head.value.toUriTemplate(t1._2), h.value.toUriTemplate(t1._3)))  
  

  implicit def toUTHPathF4[R <: RelativePathAspect,A <: CanAddAspect,P <: CanHavePrefixAspect,T1,T2,T3,T4] =
    apply[UTEncHPath4[R,A,P,T1,T2,T3,T4], (T1,T2,T3,T4)](h => t1 => sum(UriTemplate(h.head.head.head.head.path),  h.head.head.head.value.toUriTemplate(t1._1), h.head.head.value.toUriTemplate(t1._2), h.head.value.toUriTemplate(t1._3), h.value.toUriTemplate(t1._4)))  
  

  implicit def toUTHPathF5[R <: RelativePathAspect,A <: CanAddAspect,P <: CanHavePrefixAspect,T1,T2,T3,T4,T5] =
    apply[UTEncHPath5[R,A,P,T1,T2,T3,T4,T5], (T1,T2,T3,T4,T5)](h => t1 => sum(UriTemplate(h.head.head.head.head.head.path),  h.head.head.head.head.value.toUriTemplate(t1._1), h.head.head.head.value.toUriTemplate(t1._2), h.head.head.value.toUriTemplate(t1._3), h.head.value.toUriTemplate(t1._4), h.value.toUriTemplate(t1._5)))  
  

  implicit def toUTHPathF6[R <: RelativePathAspect,A <: CanAddAspect,P <: CanHavePrefixAspect,T1,T2,T3,T4,T5,T6] =
    apply[UTEncHPath6[R,A,P,T1,T2,T3,T4,T5,T6], (T1,T2,T3,T4,T5,T6)](h => t1 => sum(UriTemplate(h.head.head.head.head.head.head.path),  h.head.head.head.head.head.value.toUriTemplate(t1._1), h.head.head.head.head.value.toUriTemplate(t1._2), h.head.head.head.value.toUriTemplate(t1._3), h.head.head.value.toUriTemplate(t1._4), h.head.value.toUriTemplate(t1._5), h.value.toUriTemplate(t1._6)))  
  

  implicit def toUTHPathF7[R <: RelativePathAspect,A <: CanAddAspect,P <: CanHavePrefixAspect,T1,T2,T3,T4,T5,T6,T7] =
    apply[UTEncHPath7[R,A,P,T1,T2,T3,T4,T5,T6,T7], (T1,T2,T3,T4,T5,T6,T7)](h => t1 => sum(UriTemplate(h.head.head.head.head.head.head.head.path),  h.head.head.head.head.head.head.value.toUriTemplate(t1._1), h.head.head.head.head.head.value.toUriTemplate(t1._2), h.head.head.head.head.value.toUriTemplate(t1._3), h.head.head.head.value.toUriTemplate(t1._4), h.head.head.value.toUriTemplate(t1._5), h.head.value.toUriTemplate(t1._6), h.value.toUriTemplate(t1._7)))  
  

  implicit def toUTHPathF8[R <: RelativePathAspect,A <: CanAddAspect,P <: CanHavePrefixAspect,T1,T2,T3,T4,T5,T6,T7,T8] =
    apply[UTEncHPath8[R,A,P,T1,T2,T3,T4,T5,T6,T7,T8], (T1,T2,T3,T4,T5,T6,T7,T8)](h => t1 => sum(UriTemplate(h.head.head.head.head.head.head.head.head.path),  h.head.head.head.head.head.head.head.value.toUriTemplate(t1._1), h.head.head.head.head.head.head.value.toUriTemplate(t1._2), h.head.head.head.head.head.value.toUriTemplate(t1._3), h.head.head.head.head.value.toUriTemplate(t1._4), h.head.head.head.value.toUriTemplate(t1._5), h.head.head.value.toUriTemplate(t1._6), h.head.value.toUriTemplate(t1._7), h.value.toUriTemplate(t1._8)))  
  

  implicit def toUTHPathF9[R <: RelativePathAspect,A <: CanAddAspect,P <: CanHavePrefixAspect,T1,T2,T3,T4,T5,T6,T7,T8,T9] =
    apply[UTEncHPath9[R,A,P,T1,T2,T3,T4,T5,T6,T7,T8,T9], (T1,T2,T3,T4,T5,T6,T7,T8,T9)](h => t1 => sum(UriTemplate(h.head.head.head.head.head.head.head.head.head.path),  h.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._1), h.head.head.head.head.head.head.head.value.toUriTemplate(t1._2), h.head.head.head.head.head.head.value.toUriTemplate(t1._3), h.head.head.head.head.head.value.toUriTemplate(t1._4), h.head.head.head.head.value.toUriTemplate(t1._5), h.head.head.head.value.toUriTemplate(t1._6), h.head.head.value.toUriTemplate(t1._7), h.head.value.toUriTemplate(t1._8), h.value.toUriTemplate(t1._9)))  
  

  implicit def toUTHPathF10[R <: RelativePathAspect,A <: CanAddAspect,P <: CanHavePrefixAspect,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10] =
    apply[UTEncHPath10[R,A,P,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10)](h => t1 => sum(UriTemplate(h.head.head.head.head.head.head.head.head.head.head.path),  h.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._1), h.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._2), h.head.head.head.head.head.head.head.value.toUriTemplate(t1._3), h.head.head.head.head.head.head.value.toUriTemplate(t1._4), h.head.head.head.head.head.value.toUriTemplate(t1._5), h.head.head.head.head.value.toUriTemplate(t1._6), h.head.head.head.value.toUriTemplate(t1._7), h.head.head.value.toUriTemplate(t1._8), h.head.value.toUriTemplate(t1._9), h.value.toUriTemplate(t1._10)))  
  

  implicit def toUTHPathF11[R <: RelativePathAspect,A <: CanAddAspect,P <: CanHavePrefixAspect,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11] =
    apply[UTEncHPath11[R,A,P,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11)](h => t1 => sum(UriTemplate(h.head.head.head.head.head.head.head.head.head.head.head.path),  h.head.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._1), h.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._2), h.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._3), h.head.head.head.head.head.head.head.value.toUriTemplate(t1._4), h.head.head.head.head.head.head.value.toUriTemplate(t1._5), h.head.head.head.head.head.value.toUriTemplate(t1._6), h.head.head.head.head.value.toUriTemplate(t1._7), h.head.head.head.value.toUriTemplate(t1._8), h.head.head.value.toUriTemplate(t1._9), h.head.value.toUriTemplate(t1._10), h.value.toUriTemplate(t1._11)))  
  

  implicit def toUTHPathF12[R <: RelativePathAspect,A <: CanAddAspect,P <: CanHavePrefixAspect,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12] =
    apply[UTEncHPath12[R,A,P,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12)](h => t1 => sum(UriTemplate(h.head.head.head.head.head.head.head.head.head.head.head.head.path),  h.head.head.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._1), h.head.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._2), h.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._3), h.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._4), h.head.head.head.head.head.head.head.value.toUriTemplate(t1._5), h.head.head.head.head.head.head.value.toUriTemplate(t1._6), h.head.head.head.head.head.value.toUriTemplate(t1._7), h.head.head.head.head.value.toUriTemplate(t1._8), h.head.head.head.value.toUriTemplate(t1._9), h.head.head.value.toUriTemplate(t1._10), h.head.value.toUriTemplate(t1._11), h.value.toUriTemplate(t1._12)))  
  

  implicit def toUTHPathF13[R <: RelativePathAspect,A <: CanAddAspect,P <: CanHavePrefixAspect,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13] =
    apply[UTEncHPath13[R,A,P,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13)](h => t1 => sum(UriTemplate(h.head.head.head.head.head.head.head.head.head.head.head.head.head.path),  h.head.head.head.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._1), h.head.head.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._2), h.head.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._3), h.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._4), h.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._5), h.head.head.head.head.head.head.head.value.toUriTemplate(t1._6), h.head.head.head.head.head.head.value.toUriTemplate(t1._7), h.head.head.head.head.head.value.toUriTemplate(t1._8), h.head.head.head.head.value.toUriTemplate(t1._9), h.head.head.head.value.toUriTemplate(t1._10), h.head.head.value.toUriTemplate(t1._11), h.head.value.toUriTemplate(t1._12), h.value.toUriTemplate(t1._13)))  
  

  implicit def toUTHPathF14[R <: RelativePathAspect,A <: CanAddAspect,P <: CanHavePrefixAspect,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14] =
    apply[UTEncHPath14[R,A,P,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14)](h => t1 => sum(UriTemplate(h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.path),  h.head.head.head.head.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._1), h.head.head.head.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._2), h.head.head.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._3), h.head.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._4), h.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._5), h.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._6), h.head.head.head.head.head.head.head.value.toUriTemplate(t1._7), h.head.head.head.head.head.head.value.toUriTemplate(t1._8), h.head.head.head.head.head.value.toUriTemplate(t1._9), h.head.head.head.head.value.toUriTemplate(t1._10), h.head.head.head.value.toUriTemplate(t1._11), h.head.head.value.toUriTemplate(t1._12), h.head.value.toUriTemplate(t1._13), h.value.toUriTemplate(t1._14)))  
  

  implicit def toUTHPathF15[R <: RelativePathAspect,A <: CanAddAspect,P <: CanHavePrefixAspect,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15] =
    apply[UTEncHPath15[R,A,P,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15)](h => t1 => sum(UriTemplate(h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.path),  h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._1), h.head.head.head.head.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._2), h.head.head.head.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._3), h.head.head.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._4), h.head.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._5), h.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._6), h.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._7), h.head.head.head.head.head.head.head.value.toUriTemplate(t1._8), h.head.head.head.head.head.head.value.toUriTemplate(t1._9), h.head.head.head.head.head.value.toUriTemplate(t1._10), h.head.head.head.head.value.toUriTemplate(t1._11), h.head.head.head.value.toUriTemplate(t1._12), h.head.head.value.toUriTemplate(t1._13), h.head.value.toUriTemplate(t1._14), h.value.toUriTemplate(t1._15)))  
  

  implicit def toUTHPathF16[R <: RelativePathAspect,A <: CanAddAspect,P <: CanHavePrefixAspect,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16] =
    apply[UTEncHPath16[R,A,P,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16)](h => t1 => sum(UriTemplate(h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.path),  h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._1), h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._2), h.head.head.head.head.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._3), h.head.head.head.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._4), h.head.head.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._5), h.head.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._6), h.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._7), h.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._8), h.head.head.head.head.head.head.head.value.toUriTemplate(t1._9), h.head.head.head.head.head.head.value.toUriTemplate(t1._10), h.head.head.head.head.head.value.toUriTemplate(t1._11), h.head.head.head.head.value.toUriTemplate(t1._12), h.head.head.head.value.toUriTemplate(t1._13), h.head.head.value.toUriTemplate(t1._14), h.head.value.toUriTemplate(t1._15), h.value.toUriTemplate(t1._16)))  
  

  implicit def toUTHPathF17[R <: RelativePathAspect,A <: CanAddAspect,P <: CanHavePrefixAspect,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17] =
    apply[UTEncHPath17[R,A,P,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17)](h => t1 => sum(UriTemplate(h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.path),  h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._1), h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._2), h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._3), h.head.head.head.head.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._4), h.head.head.head.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._5), h.head.head.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._6), h.head.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._7), h.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._8), h.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._9), h.head.head.head.head.head.head.head.value.toUriTemplate(t1._10), h.head.head.head.head.head.head.value.toUriTemplate(t1._11), h.head.head.head.head.head.value.toUriTemplate(t1._12), h.head.head.head.head.value.toUriTemplate(t1._13), h.head.head.head.value.toUriTemplate(t1._14), h.head.head.value.toUriTemplate(t1._15), h.head.value.toUriTemplate(t1._16), h.value.toUriTemplate(t1._17)))  
  

  implicit def toUTHPathF18[R <: RelativePathAspect,A <: CanAddAspect,P <: CanHavePrefixAspect,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18] =
    apply[UTEncHPath18[R,A,P,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18)](h => t1 => sum(UriTemplate(h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.path),  h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._1), h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._2), h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._3), h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._4), h.head.head.head.head.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._5), h.head.head.head.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._6), h.head.head.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._7), h.head.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._8), h.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._9), h.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._10), h.head.head.head.head.head.head.head.value.toUriTemplate(t1._11), h.head.head.head.head.head.head.value.toUriTemplate(t1._12), h.head.head.head.head.head.value.toUriTemplate(t1._13), h.head.head.head.head.value.toUriTemplate(t1._14), h.head.head.head.value.toUriTemplate(t1._15), h.head.head.value.toUriTemplate(t1._16), h.head.value.toUriTemplate(t1._17), h.value.toUriTemplate(t1._18)))  
  

  implicit def toUTHPathF19[R <: RelativePathAspect,A <: CanAddAspect,P <: CanHavePrefixAspect,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19] =
    apply[UTEncHPath19[R,A,P,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19)](h => t1 => sum(UriTemplate(h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.path),  h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._1), h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._2), h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._3), h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._4), h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._5), h.head.head.head.head.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._6), h.head.head.head.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._7), h.head.head.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._8), h.head.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._9), h.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._10), h.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._11), h.head.head.head.head.head.head.head.value.toUriTemplate(t1._12), h.head.head.head.head.head.head.value.toUriTemplate(t1._13), h.head.head.head.head.head.value.toUriTemplate(t1._14), h.head.head.head.head.value.toUriTemplate(t1._15), h.head.head.head.value.toUriTemplate(t1._16), h.head.head.value.toUriTemplate(t1._17), h.head.value.toUriTemplate(t1._18), h.value.toUriTemplate(t1._19)))  
  

  implicit def toUTHPathF20[R <: RelativePathAspect,A <: CanAddAspect,P <: CanHavePrefixAspect,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20] =
    apply[UTEncHPath20[R,A,P,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20)](h => t1 => sum(UriTemplate(h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.path),  h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._1), h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._2), h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._3), h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._4), h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._5), h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._6), h.head.head.head.head.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._7), h.head.head.head.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._8), h.head.head.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._9), h.head.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._10), h.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._11), h.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._12), h.head.head.head.head.head.head.head.value.toUriTemplate(t1._13), h.head.head.head.head.head.head.value.toUriTemplate(t1._14), h.head.head.head.head.head.value.toUriTemplate(t1._15), h.head.head.head.head.value.toUriTemplate(t1._16), h.head.head.head.value.toUriTemplate(t1._17), h.head.head.value.toUriTemplate(t1._18), h.head.value.toUriTemplate(t1._19), h.value.toUriTemplate(t1._20)))  
  

  implicit def toUTHPathF21[R <: RelativePathAspect,A <: CanAddAspect,P <: CanHavePrefixAspect,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21] =
    apply[UTEncHPath21[R,A,P,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21)](h => t1 => sum(UriTemplate(h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.path),  h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._1), h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._2), h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._3), h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._4), h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._5), h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._6), h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._7), h.head.head.head.head.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._8), h.head.head.head.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._9), h.head.head.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._10), h.head.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._11), h.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._12), h.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._13), h.head.head.head.head.head.head.head.value.toUriTemplate(t1._14), h.head.head.head.head.head.head.value.toUriTemplate(t1._15), h.head.head.head.head.head.value.toUriTemplate(t1._16), h.head.head.head.head.value.toUriTemplate(t1._17), h.head.head.head.value.toUriTemplate(t1._18), h.head.head.value.toUriTemplate(t1._19), h.head.value.toUriTemplate(t1._20), h.value.toUriTemplate(t1._21)))  
  

  implicit def toUTHPathF22[R <: RelativePathAspect,A <: CanAddAspect,P <: CanHavePrefixAspect,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21,T22] =
    apply[UTEncHPath22[R,A,P,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21,T22], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21,T22)](h => t1 => sum(UriTemplate(h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.path),  h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._1), h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._2), h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._3), h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._4), h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._5), h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._6), h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._7), h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._8), h.head.head.head.head.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._9), h.head.head.head.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._10), h.head.head.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._11), h.head.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._12), h.head.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._13), h.head.head.head.head.head.head.head.head.value.toUriTemplate(t1._14), h.head.head.head.head.head.head.head.value.toUriTemplate(t1._15), h.head.head.head.head.head.head.value.toUriTemplate(t1._16), h.head.head.head.head.head.value.toUriTemplate(t1._17), h.head.head.head.head.value.toUriTemplate(t1._18), h.head.head.head.value.toUriTemplate(t1._19), h.head.head.value.toUriTemplate(t1._20), h.head.value.toUriTemplate(t1._21), h.value.toUriTemplate(t1._22)))  

}