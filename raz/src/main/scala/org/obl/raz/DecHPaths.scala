package org.obl.raz

object DecHPaths {
  
  type Cons[+H <: HPath, T] =
    HPathCons[H,_,_,T,_,_]

  type DecHPath1[T1] = Cons[HPathNil[_,_],T1]
  type DecHPath2[T1, T2] = Cons[DecHPath1[T1],T2]
  type DecHPath3[T1, T2, T3] = Cons[DecHPath2[T1, T2],T3]
  type DecHPath4[T1, T2, T3, T4] = Cons[DecHPath3[T1, T2, T3],T4]
  type DecHPath5[T1, T2, T3, T4, T5] = Cons[DecHPath4[T1, T2, T3, T4],T5]
  type DecHPath6[T1, T2, T3, T4, T5, T6] = Cons[DecHPath5[T1, T2, T3, T4, T5],T6]
  type DecHPath7[T1, T2, T3, T4, T5, T6, T7] = Cons[DecHPath6[T1, T2, T3, T4, T5, T6],T7]
  type DecHPath8[T1, T2, T3, T4, T5, T6, T7, T8] = Cons[DecHPath7[T1, T2, T3, T4, T5, T6, T7],T8]
  type DecHPath9[T1, T2, T3, T4, T5, T6, T7, T8, T9] = Cons[DecHPath8[T1, T2, T3, T4, T5, T6, T7, T8],T9]
  type DecHPath10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10] = Cons[DecHPath9[T1, T2, T3, T4, T5, T6, T7, T8, T9],T10]
  type DecHPath11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11] = Cons[DecHPath10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10],T11]
  type DecHPath12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12] = Cons[DecHPath11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11],T12]
  type DecHPath13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13] = Cons[DecHPath12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12],T13]
  type DecHPath14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14] = Cons[DecHPath13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13],T14]
  type DecHPath15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15] = Cons[DecHPath14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14],T15]
  type DecHPath16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16] = Cons[DecHPath15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15],T16]
  type DecHPath17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17] = Cons[DecHPath16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16],T17]
  type DecHPath18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18] = Cons[DecHPath17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17],T18]
  type DecHPath19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = Cons[DecHPath18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18],T19]
  type DecHPath20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20] = Cons[DecHPath19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19],T20]
  type DecHPath21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21] = Cons[DecHPath20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20],T21]
  type DecHPath22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22] = Cons[DecHPath21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21],T22]
  
}