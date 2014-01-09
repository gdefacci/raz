package org.obl.raz

trait HF[-H, Out] {
  def apply(h: H): Out
}

object HF {

  def apply[H, Out](f: H => Out) = new HF[H, Out] {
    def apply(h: H) = f(h)
  }

  import PathHelper._
  import HElems._

  private object Base extends PathFSum {
    override def pathFSum1[T1](h: HResource[HRoot, T1]): T1 => Path = { t1 =>
      sum(h.head, h.value(t1))
    }
  }

  implicit def hf1[T1] = HF[HElem1[T1], T1 => Path](h => Base.pathFSum1(h))
  implicit def hf2[T1, T2] = HF[HElem2[T1, T2], (T1, T2) => Path](h => (v1, v2) => Base.pathFSum2(h)((v1, v2)))
  implicit def hf3[T1, T2, T3] = HF[HElem3[T1, T2, T3], (T1, T2, T3) => Path](h => (v1, v2, v3) => Base.pathFSum3(h)((v1, v2, v3)))
  implicit def hf4[T1, T2, T3, T4] = HF[HElem4[T1, T2, T3, T4], (T1, T2, T3, T4) => Path](h => (v1, v2, v3, v4) => Base.pathFSum4(h)((v1, v2, v3, v4)))
  implicit def hf5[T1, T2, T3, T4, T5] = HF[HElem5[T1, T2, T3, T4, T5], (T1, T2, T3, T4, T5) => Path](h => (v1, v2, v3, v4, v5) => Base.pathFSum5(h)((v1, v2, v3, v4, v5)))
  implicit def hf6[T1, T2, T3, T4, T5, T6] = HF[HElem6[T1, T2, T3, T4, T5, T6], (T1, T2, T3, T4, T5, T6) => Path](h => (v1, v2, v3, v4, v5, v6) => Base.pathFSum6(h)((v1, v2, v3, v4, v5, v6)))
  implicit def hf7[T1, T2, T3, T4, T5, T6, T7] = HF[HElem7[T1, T2, T3, T4, T5, T6, T7], (T1, T2, T3, T4, T5, T6, T7) => Path](h => (v1, v2, v3, v4, v5, v6, v7) => Base.pathFSum7(h)((v1, v2, v3, v4, v5, v6, v7)))
  implicit def hf8[T1, T2, T3, T4, T5, T6, T7, T8] = HF[HElem8[T1, T2, T3, T4, T5, T6, T7, T8], (T1, T2, T3, T4, T5, T6, T7, T8) => Path](h => (v1, v2, v3, v4, v5, v6, v7, v8) => Base.pathFSum8(h)((v1, v2, v3, v4, v5, v6, v7, v8)))
  implicit def hf9[T1, T2, T3, T4, T5, T6, T7, T8, T9] = HF[HElem9[T1, T2, T3, T4, T5, T6, T7, T8, T9], (T1, T2, T3, T4, T5, T6, T7, T8, T9) => Path](h => (v1, v2, v3, v4, v5, v6, v7, v8, v9) => Base.pathFSum9(h)((v1, v2, v3, v4, v5, v6, v7, v8, v9)))
  implicit def hf10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10] = HF[HElem10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10], (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => Path](h => (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10) => Base.pathFSum10(h)((v1, v2, v3, v4, v5, v6, v7, v8, v9, v10)))
  implicit def hf11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11] = HF[HElem11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11], (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11) => Path](h => (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11) => Base.pathFSum11(h)((v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11)))
  implicit def hf12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12] = HF[HElem12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12], (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12) => Path](h => (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12) => Base.pathFSum12(h)((v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12)))
  implicit def hf13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13] = HF[HElem13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13], (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13) => Path](h => (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13) => Base.pathFSum13(h)((v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13)))
  implicit def hf14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14] = HF[HElem14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14], (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14) => Path](h => (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14) => Base.pathFSum14(h)((v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14)))
  implicit def hf15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15] = HF[HElem15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15], (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15) => Path](h => (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15) => Base.pathFSum15(h)((v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15)))
  implicit def hf16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16] = HF[HElem16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16], (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16) => Path](h => (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16) => Base.pathFSum16(h)((v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16)))
  implicit def hf17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17] = HF[HElem17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17], (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17) => Path](h => (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17) => Base.pathFSum17(h)((v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17)))
  implicit def hf18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18] = HF[HElem18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18], (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18) => Path](h => (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18) => Base.pathFSum18(h)((v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18)))
  implicit def hf19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = HF[HElem19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19], (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19) => Path](h => (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19) => Base.pathFSum19(h)((v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19)))
  implicit def hf20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20] = HF[HElem20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20], (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20) => Path](h => (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, v20) => Base.pathFSum20(h)((v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, v20)))
  implicit def hf21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21] = HF[HElem21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21], (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21) => Path](h => (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, v20, v21) => Base.pathFSum21(h)((v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, v20, v21)))
  implicit def hf22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22] = HF[HElem22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22], (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22) => Path](h => (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, v20, v21, v22) => Base.pathFSum22(h)((v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, v20, v21, v22)))

}
