package org.obl.raz

import HPaths._

trait HMapper[-HP <: HPath, TUP] {
  type Out[T]
  def create[P1, PTH <: Path](h: HP, cnv: Converter[TUP, P1], hf: TUP => PTH, matcher: Path => Option[PathMatchResult[TUP, Path]]): Out[P1]
}

object HMapper {

  private type Root = HPathNil[IsRelativePath, CanAddPath, CanHavePathAsPrefix]
  private val root = HPathNil[IsRelativePath, CanAddPath, CanHavePathAsPrefix](BasePath[IsRelativePath, CanAddPath, CanHavePathAsPrefix](None,PathSg.empty, Seq.empty, None))

  class BaseMapper[HP <: HPath, R <: RelativePathAspect, A <: CanAddAspect, P <: CanHavePrefixAspect, T1, TUP] extends HMapper[HPathCons[HP, R, A, P, T1], TUP] {
    type Out[T] = HPathCons[HPathNil[R,A,P], R, A, P, T]
    def create[P1, PTH <: Path](h: HPathCons[HP, R, A, P, T1], cnv: Converter[TUP, P1], hf: TUP => PTH, matcher: Path => Option[PathMatchResult[TUP, Path]]): HPathCons[HPathNil[R,A,P], R, A, P, P1] = {
      HPathConsFactory[R, A, P].create(HPathNil[R,A,P](BasePath.empty[R,A,P]), PathF[P1](p1 => hf(cnv.unapply(p1).get), pth => matcher(pth).map(mr => mr.mapValue(cnv.apply _)), h.value.suffix, h.value.expansionKind))
    }
  }

  implicit def mapper1[R <: RelativePathAspect, A <: CanAddAspect, P <: CanHavePrefixAspect, T1] = new BaseMapper[Root, R, A, P, T1, T1]
  implicit def mapper2[R <: RelativePathAspect, A <: CanAddAspect, P <: CanHavePrefixAspect, T1, T2] = new BaseMapper[HPath1[R, A, P, T1], R, A, P, T2, (T1, T2)]
  implicit def mapper3[R <: RelativePathAspect, A <: CanAddAspect, P <: CanHavePrefixAspect, T1, T2, T3] = new BaseMapper[HPath2[R, A, P, T1, T2], R, A, P, T3, (T1, T2, T3)]
  implicit def mapper4[R <: RelativePathAspect, A <: CanAddAspect, P <: CanHavePrefixAspect, T1, T2, T3, T4] = new BaseMapper[HPath3[R, A, P, T1, T2, T3], R, A, P, T4, (T1, T2, T3, T4)]
  implicit def mapper5[R <: RelativePathAspect, A <: CanAddAspect, P <: CanHavePrefixAspect, T1, T2, T3, T4, T5] = new BaseMapper[HPath4[R, A, P, T1, T2, T3, T4], R, A, P, T5, (T1, T2, T3, T4, T5)]
  implicit def mapper6[R <: RelativePathAspect, A <: CanAddAspect, P <: CanHavePrefixAspect, T1, T2, T3, T4, T5, T6] = new BaseMapper[HPath5[R, A, P, T1, T2, T3, T4, T5], R, A, P, T6, (T1, T2, T3, T4, T5, T6)]
  implicit def mapper7[R <: RelativePathAspect, A <: CanAddAspect, P <: CanHavePrefixAspect, T1, T2, T3, T4, T5, T6, T7] = new BaseMapper[HPath6[R, A, P, T1, T2, T3, T4, T5, T6], R, A, P, T7, (T1, T2, T3, T4, T5, T6, T7)]
  implicit def mapper8[R <: RelativePathAspect, A <: CanAddAspect, P <: CanHavePrefixAspect, T1, T2, T3, T4, T5, T6, T7, T8] = new BaseMapper[HPath7[R, A, P, T1, T2, T3, T4, T5, T6, T7], R, A, P, T8, (T1, T2, T3, T4, T5, T6, T7, T8)]
  implicit def mapper9[R <: RelativePathAspect, A <: CanAddAspect, P <: CanHavePrefixAspect, T1, T2, T3, T4, T5, T6, T7, T8, T9] = new BaseMapper[HPath8[R, A, P, T1, T2, T3, T4, T5, T6, T7, T8], R, A, P, T9, (T1, T2, T3, T4, T5, T6, T7, T8, T9)]
  implicit def mapper10[R <: RelativePathAspect, A <: CanAddAspect, P <: CanHavePrefixAspect, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10] = new BaseMapper[HPath9[R, A, P, T1, T2, T3, T4, T5, T6, T7, T8, T9], R, A, P, T10, (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10)]
  implicit def mapper11[R <: RelativePathAspect, A <: CanAddAspect, P <: CanHavePrefixAspect, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11] = new BaseMapper[HPath10[R, A, P, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10], R, A, P, T11, (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11)]
  implicit def mapper12[R <: RelativePathAspect, A <: CanAddAspect, P <: CanHavePrefixAspect, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12] = new BaseMapper[HPath11[R, A, P, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11], R, A, P, T12, (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12)]
  implicit def mapper13[R <: RelativePathAspect, A <: CanAddAspect, P <: CanHavePrefixAspect, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13] = new BaseMapper[HPath12[R, A, P, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12], R, A, P, T13, (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13)]
  implicit def mapper14[R <: RelativePathAspect, A <: CanAddAspect, P <: CanHavePrefixAspect, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14] = new BaseMapper[HPath13[R, A, P, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13], R, A, P, T14, (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14)]
  implicit def mapper15[R <: RelativePathAspect, A <: CanAddAspect, P <: CanHavePrefixAspect, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15] = new BaseMapper[HPath14[R, A, P, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14], R, A, P, T15, (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15)]
  implicit def mapper16[R <: RelativePathAspect, A <: CanAddAspect, P <: CanHavePrefixAspect, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16] = new BaseMapper[HPath15[R, A, P, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15], R, A, P, T16, (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16)]
  implicit def mapper17[R <: RelativePathAspect, A <: CanAddAspect, P <: CanHavePrefixAspect, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17] = new BaseMapper[HPath16[R, A, P, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16], R, A, P, T17, (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17)]
  implicit def mapper18[R <: RelativePathAspect, A <: CanAddAspect, P <: CanHavePrefixAspect, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18] = new BaseMapper[HPath17[R, A, P, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17], R, A, P, T18, (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18)]
  implicit def mapper19[R <: RelativePathAspect, A <: CanAddAspect, P <: CanHavePrefixAspect, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = new BaseMapper[HPath18[R, A, P, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18], R, A, P, T19, (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19)]
  implicit def mapper20[R <: RelativePathAspect, A <: CanAddAspect, P <: CanHavePrefixAspect, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20] = new BaseMapper[HPath19[R, A, P, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19], R, A, P, T20, (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20)]
  implicit def mapper21[R <: RelativePathAspect, A <: CanAddAspect, P <: CanHavePrefixAspect, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21] = new BaseMapper[HPath20[R, A, P, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20], R, A, P, T21, (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21)]
  implicit def mapper22[R <: RelativePathAspect, A <: CanAddAspect, P <: CanHavePrefixAspect, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22] = new BaseMapper[HPath21[R, A, P, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21], R, A, P, T22, (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22)]

}