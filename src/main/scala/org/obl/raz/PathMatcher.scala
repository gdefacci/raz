package org.obl.raz

//trait PathMatch[-H] {
//  type T
//  def apply(h: H): Path => Option[PathMatchResult[T, Path]]
//}
//
//object PathMatch {
//  implicit def fromPathMatcher[H, T1](implicit pm: PathMatcher[H, T1]) =
//    new PathMatch[H] {
//      type T = T1
//      def apply(h: H): Path => Option[PathMatchResult[T, Path]] = pm.matcher(h)
//    }
//}

trait PathMatcher[-H, +T] {
  def matcher(h: H): Path => Option[PathMatchResult[T, Path]]
}

object PathMatcher {

  import PathHelper._

  def apply[H, T](f: H => Path => Option[PathMatchResult[T, Path]]) =
    new PathMatcher[H, T] {
      def matcher(h: H): Path => Option[PathMatchResult[T, Path]] = f(h)
    }

  import HElems._

  private object Base extends BasePathMatchers {
    override def matcher1[T1](h: HElem1[T1]): Path => Option[PathMatchResult[T1, Path]] = { pth =>
      subtract(pth, h.head).flatMap { r0 =>
        h.value.matchPath(r0)
      }
    }
  }
  
//  implicit val rootPathMatcher = PathMatcher[RootPath,Path](rp => p => if (rp == p) Some(PathMatchResult(p, Raz)) else None)
//  implicit val rootUriMatcher = PathMatcher[RootUri,Path](rp => p => if (rp == p) Some(PathMatchResult(p, Raz)) else None)
//  implicit val rootParamsMatcher = PathMatcher[RootParams,Path](rp => p => if (rp == p) Some(PathMatchResult(p, Raz)) else None)
  
//  implicit val rootResourceMatcher = PathMatcher[Path, Path](rp => p => if (rp == p) Some(PathMatchResult(p, Raz)) else None)
  implicit def rootResourceMatcher[H <: HRoot] = PathMatcher[H, Path](rp => p => if (rp == p) Some(PathMatchResult(p, Raz)) else None)

  implicit def matcher1[T1]: PathMatcher[HElem1[T1], T1] = {
    apply[HElem1[T1], T1](Base.matcher1)
  }

  implicit def matcher2[T1, T2]: PathMatcher[HElem2[T1, T2], (T1, T2)] = {
    apply[HElem2[T1, T2], (T1, T2)](Base.matcher2)
  }

  implicit def matcher3[T1, T2, T3]: PathMatcher[HElem3[T1, T2, T3], (T1, T2, T3)] = {
    apply[HElem3[T1, T2, T3], (T1, T2, T3)](Base.matcher3)
  }

  implicit def matcher4[T1, T2, T3, T4]: PathMatcher[HElem4[T1, T2, T3, T4], (T1, T2, T3, T4)] = {
    apply[HElem4[T1, T2, T3, T4], (T1, T2, T3, T4)](Base.matcher4)
  }

  implicit def matcher5[T1, T2, T3, T4, T5]: PathMatcher[HElem5[T1, T2, T3, T4, T5], (T1, T2, T3, T4, T5)] = {
    apply[HElem5[T1, T2, T3, T4, T5], (T1, T2, T3, T4, T5)](Base.matcher5)
  }

  implicit def matcher6[T1, T2, T3, T4, T5, T6]: PathMatcher[HElem6[T1, T2, T3, T4, T5, T6], (T1, T2, T3, T4, T5, T6)] = {
    apply[HElem6[T1, T2, T3, T4, T5, T6], (T1, T2, T3, T4, T5, T6)](Base.matcher6)
  }

  implicit def matcher7[T1, T2, T3, T4, T5, T6, T7]: PathMatcher[HElem7[T1, T2, T3, T4, T5, T6, T7], (T1, T2, T3, T4, T5, T6, T7)] = {
    apply[HElem7[T1, T2, T3, T4, T5, T6, T7], (T1, T2, T3, T4, T5, T6, T7)](Base.matcher7)
  }

  implicit def matcher8[T1, T2, T3, T4, T5, T6, T7, T8]: PathMatcher[HElem8[T1, T2, T3, T4, T5, T6, T7, T8], (T1, T2, T3, T4, T5, T6, T7, T8)] = {
    apply[HElem8[T1, T2, T3, T4, T5, T6, T7, T8], (T1, T2, T3, T4, T5, T6, T7, T8)](Base.matcher8)
  }

  implicit def matcher9[T1, T2, T3, T4, T5, T6, T7, T8, T9]: PathMatcher[HElem9[T1, T2, T3, T4, T5, T6, T7, T8, T9], (T1, T2, T3, T4, T5, T6, T7, T8, T9)] = {
    apply[HElem9[T1, T2, T3, T4, T5, T6, T7, T8, T9], (T1, T2, T3, T4, T5, T6, T7, T8, T9)](Base.matcher9)
  }

  implicit def matcher10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]: PathMatcher[HElem10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10], (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10)] = {
    apply[HElem10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10], (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10)](Base.matcher10)
  }

  implicit def matcher11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]: PathMatcher[HElem11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11], (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11)] = {
    apply[HElem11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11], (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11)](Base.matcher11)
  }

  implicit def matcher12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]: PathMatcher[HElem12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12], (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12)] = {
    apply[HElem12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12], (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12)](Base.matcher12)
  }

  implicit def matcher13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]: PathMatcher[HElem13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13], (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13)] = {
    apply[HElem13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13], (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13)](Base.matcher13)
  }

  implicit def matcher14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]: PathMatcher[HElem14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14], (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14)] = {
    apply[HElem14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14], (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14)](Base.matcher14)
  }

  implicit def matcher15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]: PathMatcher[HElem15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15], (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15)] = {
    apply[HElem15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15], (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15)](Base.matcher15)
  }

  implicit def matcher16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]: PathMatcher[HElem16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16], (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16)] = {
    apply[HElem16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16], (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16)](Base.matcher16)
  }

  implicit def matcher17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]: PathMatcher[HElem17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17], (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17)] = {
    apply[HElem17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17], (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17)](Base.matcher17)
  }

  implicit def matcher18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]: PathMatcher[HElem18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18], (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18)] = {
    apply[HElem18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18], (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18)](Base.matcher18)
  }

  implicit def matcher19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]: PathMatcher[HElem19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19], (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19)] = {
    apply[HElem19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19], (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19)](Base.matcher19)
  }

  implicit def matcher20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]: PathMatcher[HElem20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20], (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20)] = {
    apply[HElem20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20], (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20)](Base.matcher20)
  }

  implicit def matcher21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]: PathMatcher[HElem21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21], (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21)] = {
    apply[HElem21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21], (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21)](Base.matcher21)
  }

  implicit def matcher22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]: PathMatcher[HElem22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22], (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22)] = {
    apply[HElem22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22], (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22)](Base.matcher22)
  }

}