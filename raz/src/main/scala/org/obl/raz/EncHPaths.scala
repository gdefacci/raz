package org.obl.raz

object EncHPaths {

  type EncHPath1[+P <: PathPosition, S <: P,T1] = HPathCons[HPathNil[_,_],P,S,_,T1,_]
  type EncHPath2[+P <: PathPosition, S <: P,T1, T2] = HPathCons[EncHPath1[_,_,T1],P,S,_,T2,_]
  type EncHPath3[+P <: PathPosition, S <: P,T1, T2, T3] = HPathCons[EncHPath2[_,_,T1, T2],P,S,_,T3,_]
  type EncHPath4[+P <: PathPosition, S <: P,T1, T2, T3, T4] = HPathCons[EncHPath3[_,_,T1, T2, T3],P,S,_,T4,_]
  type EncHPath5[+P <: PathPosition, S <: P,T1, T2, T3, T4, T5] = HPathCons[EncHPath4[_,_,T1, T2, T3, T4],P,S,_,T5,_]
  type EncHPath6[+P <: PathPosition, S <: P,T1, T2, T3, T4, T5, T6] = HPathCons[EncHPath5[_,_,T1, T2, T3, T4, T5],P,S,_,T6,_]
  type EncHPath7[+P <: PathPosition, S <: P,T1, T2, T3, T4, T5, T6, T7] = HPathCons[EncHPath6[_,_,T1, T2, T3, T4, T5, T6],P,S,_,T7,_]
  type EncHPath8[+P <: PathPosition, S <: P,T1, T2, T3, T4, T5, T6, T7, T8] = HPathCons[EncHPath7[_,_,T1, T2, T3, T4, T5, T6, T7],P,S,_,T8,_]
  type EncHPath9[+P <: PathPosition, S <: P,T1, T2, T3, T4, T5, T6, T7, T8, T9] = HPathCons[EncHPath8[_,_,T1, T2, T3, T4, T5, T6, T7, T8],P,S,_,T9,_]
  type EncHPath10[+P <: PathPosition, S <: P,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10] = HPathCons[EncHPath9[_,_,T1, T2, T3, T4, T5, T6, T7, T8, T9],P,S,_,T10,_]
  type EncHPath11[+P <: PathPosition, S <: P,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11] = HPathCons[EncHPath10[_,_,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10],P,S,_,T11,_]
  type EncHPath12[+P <: PathPosition, S <: P,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12] = HPathCons[EncHPath11[_,_,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11],P,S,_,T12,_]
  type EncHPath13[+P <: PathPosition, S <: P,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13] = HPathCons[EncHPath12[_,_,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12],P,S,_,T13,_]
  type EncHPath14[+P <: PathPosition, S <: P,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14] = HPathCons[EncHPath13[_,_,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13],P,S,_,T14,_]
  type EncHPath15[+P <: PathPosition, S <: P,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15] = HPathCons[EncHPath14[_,_,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14],P,S,_,T15,_]
  type EncHPath16[+P <: PathPosition, S <: P,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16] = HPathCons[EncHPath15[_,_,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15],P,S,_,T16,_]
  type EncHPath17[+P <: PathPosition, S <: P,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17] = HPathCons[EncHPath16[_,_,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16],P,S,_,T17,_]
  type EncHPath18[+P <: PathPosition, S <: P,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18] = HPathCons[EncHPath17[_,_,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17],P,S,_,T18,_]
  type EncHPath19[+P <: PathPosition, S <: P,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = HPathCons[EncHPath18[_,_,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18],P,S,_,T19,_]
  type EncHPath20[+P <: PathPosition, S <: P,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20] = HPathCons[EncHPath19[_,_,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19],P,S,_,T20,_]
  type EncHPath21[+P <: PathPosition, S <: P,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21] = HPathCons[EncHPath20[_,_,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20],P,S,_,T21,_]
  type EncHPath22[+P <: PathPosition, S <: P,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22] = HPathCons[EncHPath21[_,_,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21],P,S,_,T22,_]
  
}