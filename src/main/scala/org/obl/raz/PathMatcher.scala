package org.obl.raz

trait PathMatcher[-H, T] {
  def matcher(h: H): Path => Option[PathMatchResult[T, Path]]
}

object PathMatcher {

  def apply[H, T](f: H => Path => Option[PathMatchResult[T, Path]]) =
    new PathMatcher[H, T] {
      def matcher(h: H): Path => Option[PathMatchResult[T, Path]] = f(h)
    }

  import HPaths._

  implicit def matcher1[T1]: PathMatcher[HPath1[_, _, _, T1], T1] = {
    apply[HPath1[_, _, _, T1], T1](Matchers.matcher1)
  }

  implicit def matcher2[T1, T2]: PathMatcher[HPath2[_, _, _, T1, T2], (T1, T2)] = {
    apply[HPath2[_, _, _, T1, T2], (T1, T2)](Matchers.matcher2)
  }

  implicit def matcher3[T1, T2, T3]: PathMatcher[HPath3[_, _, _, T1, T2, T3], (T1, T2, T3)] = {
    apply[HPath3[_, _, _, T1, T2, T3], (T1, T2, T3)](Matchers.matcher3)
  }

  implicit def matcher4[T1, T2, T3, T4]: PathMatcher[HPath4[_, _, _, T1, T2, T3, T4], (T1, T2, T3, T4)] = {
    apply[HPath4[_, _, _, T1, T2, T3, T4], (T1, T2, T3, T4)](Matchers.matcher4)
  }

  implicit def matcher5[T1, T2, T3, T4, T5]: PathMatcher[HPath5[_, _, _, T1, T2, T3, T4, T5], (T1, T2, T3, T4, T5)] = {
    apply[HPath5[_, _, _, T1, T2, T3, T4, T5], (T1, T2, T3, T4, T5)](Matchers.matcher5)
  }

  implicit def matcher6[T1, T2, T3, T4, T5, T6]: PathMatcher[HPath6[_, _, _, T1, T2, T3, T4, T5, T6], (T1, T2, T3, T4, T5, T6)] = {
    apply[HPath6[_, _, _, T1, T2, T3, T4, T5, T6], (T1, T2, T3, T4, T5, T6)](Matchers.matcher6)
  }

  implicit def matcher7[T1, T2, T3, T4, T5, T6, T7]: PathMatcher[HPath7[_, _, _, T1, T2, T3, T4, T5, T6, T7], (T1, T2, T3, T4, T5, T6, T7)] = {
    apply[HPath7[_, _, _, T1, T2, T3, T4, T5, T6, T7], (T1, T2, T3, T4, T5, T6, T7)](Matchers.matcher7)
  }

  implicit def matcher8[T1, T2, T3, T4, T5, T6, T7, T8]: PathMatcher[HPath8[_, _, _, T1, T2, T3, T4, T5, T6, T7, T8], (T1, T2, T3, T4, T5, T6, T7, T8)] = {
    apply[HPath8[_, _, _, T1, T2, T3, T4, T5, T6, T7, T8], (T1, T2, T3, T4, T5, T6, T7, T8)](Matchers.matcher8)
  }

  implicit def matcher9[T1, T2, T3, T4, T5, T6, T7, T8, T9]: PathMatcher[HPath9[_, _, _, T1, T2, T3, T4, T5, T6, T7, T8, T9], (T1, T2, T3, T4, T5, T6, T7, T8, T9)] = {
    apply[HPath9[_, _, _, T1, T2, T3, T4, T5, T6, T7, T8, T9], (T1, T2, T3, T4, T5, T6, T7, T8, T9)](Matchers.matcher9)
  }

  implicit def matcher10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]: PathMatcher[HPath10[_, _, _, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10], (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10)] = {
    apply[HPath10[_, _, _, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10], (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10)](Matchers.matcher10)
  }

  implicit def matcher11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]: PathMatcher[HPath11[_, _, _, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11], (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11)] = {
    apply[HPath11[_, _, _, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11], (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11)](Matchers.matcher11)
  }

  implicit def matcher12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]: PathMatcher[HPath12[_, _, _, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12], (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12)] = {
    apply[HPath12[_, _, _, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12], (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12)](Matchers.matcher12)
  }

  implicit def matcher13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]: PathMatcher[HPath13[_, _, _, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13], (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13)] = {
    apply[HPath13[_, _, _, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13], (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13)](Matchers.matcher13)
  }

  implicit def matcher14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]: PathMatcher[HPath14[_, _, _, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14], (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14)] = {
    apply[HPath14[_, _, _, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14], (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14)](Matchers.matcher14)
  }

  implicit def matcher15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]: PathMatcher[HPath15[_, _, _, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15], (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15)] = {
    apply[HPath15[_, _, _, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15], (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15)](Matchers.matcher15)
  }

  implicit def matcher16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]: PathMatcher[HPath16[_, _, _, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16], (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16)] = {
    apply[HPath16[_, _, _, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16], (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16)](Matchers.matcher16)
  }

  implicit def matcher17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]: PathMatcher[HPath17[_, _, _, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17], (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17)] = {
    apply[HPath17[_, _, _, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17], (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17)](Matchers.matcher17)
  }

  implicit def matcher18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]: PathMatcher[HPath18[_, _, _, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18], (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18)] = {
    apply[HPath18[_, _, _, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18], (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18)](Matchers.matcher18)
  }

  implicit def matcher19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]: PathMatcher[HPath19[_, _, _, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19], (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19)] = {
    apply[HPath19[_, _, _, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19], (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19)](Matchers.matcher19)
  }

  implicit def matcher20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]: PathMatcher[HPath20[_, _, _, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20], (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20)] = {
    apply[HPath20[_, _, _, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20], (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20)](Matchers.matcher20)
  }

  implicit def matcher21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]: PathMatcher[HPath21[_, _, _, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21], (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21)] = {
    apply[HPath21[_, _, _, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21], (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21)](Matchers.matcher21)
  }

  implicit def matcher22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]: PathMatcher[HPath22[_, _, _, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22], (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22)] = {
    apply[HPath22[_, _, _, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22], (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22)](Matchers.matcher22)
  }

}