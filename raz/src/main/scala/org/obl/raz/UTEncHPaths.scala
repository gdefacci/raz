package org.obl.raz

object UTEncHPaths {

  type UTEncHPath1[+P <: PathPosition, S <: P,T1] = HPathCons[HPathNil[_,_],P,S,_,_,T1]
  type UTEncHPath2[+P <: PathPosition, S <: P,T1, T2] = HPathCons[UTEncHPath1[_,_,T1],P,S,_,_,T2]
  type UTEncHPath3[+P <: PathPosition, S <: P,T1, T2, T3] = HPathCons[UTEncHPath2[_,_,T1, T2],P,S,_,_,T3]
  type UTEncHPath4[+P <: PathPosition, S <: P,T1, T2, T3, T4] = HPathCons[UTEncHPath3[_,_,T1, T2, T3],P,S,_,_,T4]
  type UTEncHPath5[+P <: PathPosition, S <: P,T1, T2, T3, T4, T5] = HPathCons[UTEncHPath4[_,_,T1, T2, T3, T4],P,S,_,_,T5]
  type UTEncHPath6[+P <: PathPosition, S <: P,T1, T2, T3, T4, T5, T6] = HPathCons[UTEncHPath5[_,_,T1, T2, T3, T4, T5],P,S,_,_,T6]
  type UTEncHPath7[+P <: PathPosition, S <: P,T1, T2, T3, T4, T5, T6, T7] = HPathCons[UTEncHPath6[_,_,T1, T2, T3, T4, T5, T6],P,S,_,_,T7]
  type UTEncHPath8[+P <: PathPosition, S <: P,T1, T2, T3, T4, T5, T6, T7, T8] = HPathCons[UTEncHPath7[_,_,T1, T2, T3, T4, T5, T6, T7],P,S,_,_,T8]
  type UTEncHPath9[+P <: PathPosition, S <: P,T1, T2, T3, T4, T5, T6, T7, T8, T9] = HPathCons[UTEncHPath8[_,_,T1, T2, T3, T4, T5, T6, T7, T8],P,S,_,_,T9]
  type UTEncHPath10[+P <: PathPosition, S <: P,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10] = HPathCons[UTEncHPath9[_,_,T1, T2, T3, T4, T5, T6, T7, T8, T9],P,S,_,_,T10]
  type UTEncHPath11[+P <: PathPosition, S <: P,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11] = HPathCons[UTEncHPath10[_,_,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10],P,S,_,_,T11]
  type UTEncHPath12[+P <: PathPosition, S <: P,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12] = HPathCons[UTEncHPath11[_,_,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11],P,S,_,_,T12]
  type UTEncHPath13[+P <: PathPosition, S <: P,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13] = HPathCons[UTEncHPath12[_,_,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12],P,S,_,_,T13]
  type UTEncHPath14[+P <: PathPosition, S <: P,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14] = HPathCons[UTEncHPath13[_,_,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13],P,S,_,_,T14]
  type UTEncHPath15[+P <: PathPosition, S <: P,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15] = HPathCons[UTEncHPath14[_,_,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14],P,S,_,_,T15]
  type UTEncHPath16[+P <: PathPosition, S <: P,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16] = HPathCons[UTEncHPath15[_,_,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15],P,S,_,_,T16]
  type UTEncHPath17[+P <: PathPosition, S <: P,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17] = HPathCons[UTEncHPath16[_,_,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16],P,S,_,_,T17]
  type UTEncHPath18[+P <: PathPosition, S <: P,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18] = HPathCons[UTEncHPath17[_,_,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17],P,S,_,_,T18]
  type UTEncHPath19[+P <: PathPosition, S <: P,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = HPathCons[UTEncHPath18[_,_,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18],P,S,_,_,T19]
  type UTEncHPath20[+P <: PathPosition, S <: P,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20] = HPathCons[UTEncHPath19[_,_,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19],P,S,_,_,T20]
  type UTEncHPath21[+P <: PathPosition, S <: P,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21] = HPathCons[UTEncHPath20[_,_,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20],P,S,_,_,T21]
  type UTEncHPath22[+P <: PathPosition, S <: P,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22] = HPathCons[UTEncHPath21[_,_,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21],P,S,_,_,T22]
  
}