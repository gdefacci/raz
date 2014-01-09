package org.obl.raz

trait HElems {

  type HElemRoot <: HRoot
  type HElem1[+T1] = HResource[HElemRoot, T1]
  type HElem2[+T1, +T2] = HResource[HElem1[T1], T2]
  type HElem3[+T1, +T2, +T3] = HResource[HElem2[T1, T2], T3]
  type HElem4[+T1, +T2, +T3, +T4] = HResource[HElem3[T1, T2, T3], T4]
  type HElem5[+T1, +T2, +T3, +T4, +T5] = HResource[HElem4[T1, T2, T3, T4], T5]
  type HElem6[+T1, +T2, +T3, +T4, +T5, +T6] = HResource[HElem5[T1, T2, T3, T4, T5], T6]
  type HElem7[+T1, +T2, +T3, +T4, +T5, +T6, +T7] = HResource[HElem6[T1, T2, T3, T4, T5, T6], T7]
  type HElem8[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8] = HResource[HElem7[T1, T2, T3, T4, T5, T6, T7], T8]
  type HElem9[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9] = HResource[HElem8[T1, T2, T3, T4, T5, T6, T7, T8], T9]
  type HElem10[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10] = HResource[HElem9[T1, T2, T3, T4, T5, T6, T7, T8, T9], T10]
  type HElem11[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11] = HResource[HElem10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10], T11]
  type HElem12[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12] = HResource[HElem11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11], T12]
  type HElem13[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13] = HResource[HElem12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12], T13]
  type HElem14[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14] = HResource[HElem13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13], T14]
  type HElem15[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14, +T15] = HResource[HElem14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14], T15]
  type HElem16[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14, +T15, +T16] = HResource[HElem15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15], T16]
  type HElem17[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14, +T15, +T16, +T17] = HResource[HElem16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16], T17]
  type HElem18[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14, +T15, +T16, +T17, +T18] = HResource[HElem17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17], T18]
  type HElem19[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14, +T15, +T16, +T17, +T18, +T19] = HResource[HElem18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18], T19]
  type HElem20[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14, +T15, +T16, +T17, +T18, +T19, +T20] = HResource[HElem19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19], T20]
  type HElem21[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14, +T15, +T16, +T17, +T18, +T19, +T20, +T21] = HResource[HElem20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20], T21]
  type HElem22[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14, +T15, +T16, +T17, +T18, +T19, +T20, +T21, +T22] = HResource[HElem21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21], T22]

}

object HElems extends HElems {
  type HElemRoot = HRoot
}

trait HParamsElems extends HElems {

  type HElemRoot <: HRoot

  type HParamsElem1[+T1] = ParamsHResource[HElemRoot, T1]
  type HParamsElem2[+T1, +T2] = ParamsHResource[HElem1[T1], T2]
  type HParamsElem3[+T1, +T2, +T3] = ParamsHResource[HElem2[T1, T2], T3]
  type HParamsElem4[+T1, +T2, +T3, +T4] = ParamsHResource[HElem3[T1, T2, T3], T4]
  type HParamsElem5[+T1, +T2, +T3, +T4, +T5] = ParamsHResource[HElem4[T1, T2, T3, T4], T5]
  type HParamsElem6[+T1, +T2, +T3, +T4, +T5, +T6] = ParamsHResource[HElem5[T1, T2, T3, T4, T5], T6]
  type HParamsElem7[+T1, +T2, +T3, +T4, +T5, +T6, +T7] = ParamsHResource[HElem6[T1, T2, T3, T4, T5, T6], T7]
  type HParamsElem8[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8] = ParamsHResource[HElem7[T1, T2, T3, T4, T5, T6, T7], T8]
  type HParamsElem9[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9] = ParamsHResource[HElem8[T1, T2, T3, T4, T5, T6, T7, T8], T9]
  type HParamsElem10[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10] = ParamsHResource[HElem9[T1, T2, T3, T4, T5, T6, T7, T8, T9], T10]
  type HParamsElem11[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11] = ParamsHResource[HElem10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10], T11]
  type HParamsElem12[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12] = ParamsHResource[HElem11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11], T12]
  type HParamsElem13[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13] = ParamsHResource[HElem12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12], T13]
  type HParamsElem14[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14] = ParamsHResource[HElem13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13], T14]
  type HParamsElem15[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14, +T15] = ParamsHResource[HElem14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14], T15]
  type HParamsElem16[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14, +T15, +T16] = ParamsHResource[HElem15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15], T16]
  type HParamsElem17[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14, +T15, +T16, +T17] = ParamsHResource[HElem16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16], T17]
  type HParamsElem18[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14, +T15, +T16, +T17, +T18] = ParamsHResource[HElem17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17], T18]
  type HParamsElem19[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14, +T15, +T16, +T17, +T18, +T19] = ParamsHResource[HElem18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18], T19]
  type HParamsElem20[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14, +T15, +T16, +T17, +T18, +T19, +T20] = ParamsHResource[HElem19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19], T20]
  type HParamsElem21[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14, +T15, +T16, +T17, +T18, +T19, +T20, +T21] = ParamsHResource[HElem20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20], T21]
  type HParamsElem22[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14, +T15, +T16, +T17, +T18, +T19, +T20, +T21, +T22] = ParamsHResource[HElem21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21], T22]

}

object HParamsElems {

  object RootPath extends HParamsElems {
	  type HElemRoot = RootPath
	}
	
	object RootUri extends HParamsElems {
	  type HElemRoot = RootUri
	}
	
	object RootParams extends HParamsElems {
	  type HElemRoot = RootParams
	}
}

object HPathElems {
  type HPathElem1[+T1] = PathHResource[RootPath, T1]
  type HPathElem2[+T1, +T2] = PathHResource[HPathElem1[T1], T2]
  type HPathElem3[+T1, +T2, +T3] = PathHResource[HPathElem2[T1, T2], T3]
  type HPathElem4[+T1, +T2, +T3, +T4] = PathHResource[HPathElem3[T1, T2, T3], T4]
  type HPathElem5[+T1, +T2, +T3, +T4, +T5] = PathHResource[HPathElem4[T1, T2, T3, T4], T5]
  type HPathElem6[+T1, +T2, +T3, +T4, +T5, +T6] = PathHResource[HPathElem5[T1, T2, T3, T4, T5], T6]
  type HPathElem7[+T1, +T2, +T3, +T4, +T5, +T6, +T7] = PathHResource[HPathElem6[T1, T2, T3, T4, T5, T6], T7]
  type HPathElem8[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8] = PathHResource[HPathElem7[T1, T2, T3, T4, T5, T6, T7], T8]
  type HPathElem9[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9] = PathHResource[HPathElem8[T1, T2, T3, T4, T5, T6, T7, T8], T9]
  type HPathElem10[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10] = PathHResource[HPathElem9[T1, T2, T3, T4, T5, T6, T7, T8, T9], T10]
  type HPathElem11[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11] = PathHResource[HPathElem10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10], T11]
  type HPathElem12[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12] = PathHResource[HPathElem11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11], T12]
  type HPathElem13[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13] = PathHResource[HPathElem12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12], T13]
  type HPathElem14[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14] = PathHResource[HPathElem13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13], T14]
  type HPathElem15[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14, +T15] = PathHResource[HPathElem14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14], T15]
  type HPathElem16[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14, +T15, +T16] = PathHResource[HPathElem15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15], T16]
  type HPathElem17[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14, +T15, +T16, +T17] = PathHResource[HPathElem16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16], T17]
  type HPathElem18[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14, +T15, +T16, +T17, +T18] = PathHResource[HPathElem17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17], T18]
  type HPathElem19[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14, +T15, +T16, +T17, +T18, +T19] = PathHResource[HPathElem18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18], T19]
  type HPathElem20[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14, +T15, +T16, +T17, +T18, +T19, +T20] = PathHResource[HPathElem19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19], T20]
  type HPathElem21[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14, +T15, +T16, +T17, +T18, +T19, +T20, +T21] = PathHResource[HPathElem20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20], T21]
  type HPathElem22[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14, +T15, +T16, +T17, +T18, +T19, +T20, +T21, +T22] = PathHResource[HPathElem21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21], T22]

}

object HPathAndParamsElems {
  import HPathElems._

  type HPathAndParamsElem1[+T1] = PathAndParamsHResource[RootPath, T1]
  type HPathAndParamsElem2[+T1, +T2] = PathAndParamsHResource[HPathElem1[T1], T2]
  type HPathAndParamsElem3[+T1, +T2, +T3] = PathAndParamsHResource[HPathElem2[T1, T2], T3]
  type HPathAndParamsElem4[+T1, +T2, +T3, +T4] = PathAndParamsHResource[HPathElem3[T1, T2, T3], T4]
  type HPathAndParamsElem5[+T1, +T2, +T3, +T4, +T5] = PathAndParamsHResource[HPathElem4[T1, T2, T3, T4], T5]
  type HPathAndParamsElem6[+T1, +T2, +T3, +T4, +T5, +T6] = PathAndParamsHResource[HPathElem5[T1, T2, T3, T4, T5], T6]
  type HPathAndParamsElem7[+T1, +T2, +T3, +T4, +T5, +T6, +T7] = PathAndParamsHResource[HPathElem6[T1, T2, T3, T4, T5, T6], T7]
  type HPathAndParamsElem8[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8] = PathAndParamsHResource[HPathElem7[T1, T2, T3, T4, T5, T6, T7], T8]
  type HPathAndParamsElem9[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9] = PathAndParamsHResource[HPathElem8[T1, T2, T3, T4, T5, T6, T7, T8], T9]
  type HPathAndParamsElem10[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10] = PathAndParamsHResource[HPathElem9[T1, T2, T3, T4, T5, T6, T7, T8, T9], T10]
  type HPathAndParamsElem11[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11] = PathAndParamsHResource[HPathElem10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10], T11]
  type HPathAndParamsElem12[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12] = PathAndParamsHResource[HPathElem11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11], T12]
  type HPathAndParamsElem13[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13] = PathAndParamsHResource[HPathElem12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12], T13]
  type HPathAndParamsElem14[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14] = PathAndParamsHResource[HPathElem13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13], T14]
  type HPathAndParamsElem15[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14, +T15] = PathAndParamsHResource[HPathElem14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14], T15]
  type HPathAndParamsElem16[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14, +T15, +T16] = PathAndParamsHResource[HPathElem15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15], T16]
  type HPathAndParamsElem17[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14, +T15, +T16, +T17] = PathAndParamsHResource[HPathElem16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16], T17]
  type HPathAndParamsElem18[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14, +T15, +T16, +T17, +T18] = PathAndParamsHResource[HPathElem17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17], T18]
  type HPathAndParamsElem19[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14, +T15, +T16, +T17, +T18, +T19] = PathAndParamsHResource[HPathElem18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18], T19]
  type HPathAndParamsElem20[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14, +T15, +T16, +T17, +T18, +T19, +T20] = PathAndParamsHResource[HPathElem19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19], T20]
  type HPathAndParamsElem21[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14, +T15, +T16, +T17, +T18, +T19, +T20, +T21] = PathAndParamsHResource[HPathElem20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20], T21]
  type HPathAndParamsElem22[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14, +T15, +T16, +T17, +T18, +T19, +T20, +T21, +T22] = PathAndParamsHResource[HPathElem21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21], T22]

}

