package org.obl.raz

object UTEncHPaths {

  type UTEncHPath1[+R <: RelativePathAspect,A <: CanAddAspect,+P <: CanHavePrefixAspect,T1] = HPathCons[HPathNil[_,_,_],R,A,P,_,_,T1]
  type UTEncHPath2[+R <: RelativePathAspect,A <: CanAddAspect,+P <: CanHavePrefixAspect,T1, T2] = HPathCons[UTEncHPath1[_,_,_,T1],R,A,P,_,_,T2]
  type UTEncHPath3[+R <: RelativePathAspect,A <: CanAddAspect,+P <: CanHavePrefixAspect,T1, T2, T3] = HPathCons[UTEncHPath2[_,_,_,T1, T2],R,A,P,_,_,T3]
  type UTEncHPath4[+R <: RelativePathAspect,A <: CanAddAspect,+P <: CanHavePrefixAspect,T1, T2, T3, T4] = HPathCons[UTEncHPath3[_,_,_,T1, T2, T3],R,A,P,_,_,T4]
  type UTEncHPath5[+R <: RelativePathAspect,A <: CanAddAspect,+P <: CanHavePrefixAspect,T1, T2, T3, T4, T5] = HPathCons[UTEncHPath4[_,_,_,T1, T2, T3, T4],R,A,P,_,_,T5]
  type UTEncHPath6[+R <: RelativePathAspect,A <: CanAddAspect,+P <: CanHavePrefixAspect,T1, T2, T3, T4, T5, T6] = HPathCons[UTEncHPath5[_,_,_,T1, T2, T3, T4, T5],R,A,P,_,_,T6]
  type UTEncHPath7[+R <: RelativePathAspect,A <: CanAddAspect,+P <: CanHavePrefixAspect,T1, T2, T3, T4, T5, T6, T7] = HPathCons[UTEncHPath6[_,_,_,T1, T2, T3, T4, T5, T6],R,A,P,_,_,T7]
  type UTEncHPath8[+R <: RelativePathAspect,A <: CanAddAspect,+P <: CanHavePrefixAspect,T1, T2, T3, T4, T5, T6, T7, T8] = HPathCons[UTEncHPath7[_,_,_,T1, T2, T3, T4, T5, T6, T7],R,A,P,_,_,T8]
  type UTEncHPath9[+R <: RelativePathAspect,A <: CanAddAspect,+P <: CanHavePrefixAspect,T1, T2, T3, T4, T5, T6, T7, T8, T9] = HPathCons[UTEncHPath8[_,_,_,T1, T2, T3, T4, T5, T6, T7, T8],R,A,P,_,_,T9]
  type UTEncHPath10[+R <: RelativePathAspect,A <: CanAddAspect,+P <: CanHavePrefixAspect,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10] = HPathCons[UTEncHPath9[_,_,_,T1, T2, T3, T4, T5, T6, T7, T8, T9],R,A,P,_,_,T10]
  type UTEncHPath11[+R <: RelativePathAspect,A <: CanAddAspect,+P <: CanHavePrefixAspect,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11] = HPathCons[UTEncHPath10[_,_,_,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10],R,A,P,_,_,T11]
  type UTEncHPath12[+R <: RelativePathAspect,A <: CanAddAspect,+P <: CanHavePrefixAspect,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12] = HPathCons[UTEncHPath11[_,_,_,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11],R,A,P,_,_,T12]
  type UTEncHPath13[+R <: RelativePathAspect,A <: CanAddAspect,+P <: CanHavePrefixAspect,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13] = HPathCons[UTEncHPath12[_,_,_,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12],R,A,P,_,_,T13]
  type UTEncHPath14[+R <: RelativePathAspect,A <: CanAddAspect,+P <: CanHavePrefixAspect,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14] = HPathCons[UTEncHPath13[_,_,_,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13],R,A,P,_,_,T14]
  type UTEncHPath15[+R <: RelativePathAspect,A <: CanAddAspect,+P <: CanHavePrefixAspect,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15] = HPathCons[UTEncHPath14[_,_,_,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14],R,A,P,_,_,T15]
  type UTEncHPath16[+R <: RelativePathAspect,A <: CanAddAspect,+P <: CanHavePrefixAspect,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16] = HPathCons[UTEncHPath15[_,_,_,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15],R,A,P,_,_,T16]
  type UTEncHPath17[+R <: RelativePathAspect,A <: CanAddAspect,+P <: CanHavePrefixAspect,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17] = HPathCons[UTEncHPath16[_,_,_,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16],R,A,P,_,_,T17]
  type UTEncHPath18[+R <: RelativePathAspect,A <: CanAddAspect,+P <: CanHavePrefixAspect,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18] = HPathCons[UTEncHPath17[_,_,_,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17],R,A,P,_,_,T18]
  type UTEncHPath19[+R <: RelativePathAspect,A <: CanAddAspect,+P <: CanHavePrefixAspect,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = HPathCons[UTEncHPath18[_,_,_,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18],R,A,P,_,_,T19]
  type UTEncHPath20[+R <: RelativePathAspect,A <: CanAddAspect,+P <: CanHavePrefixAspect,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20] = HPathCons[UTEncHPath19[_,_,_,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19],R,A,P,_,_,T20]
  type UTEncHPath21[+R <: RelativePathAspect,A <: CanAddAspect,+P <: CanHavePrefixAspect,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21] = HPathCons[UTEncHPath20[_,_,_,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20],R,A,P,_,_,T21]
  type UTEncHPath22[+R <: RelativePathAspect,A <: CanAddAspect,+P <: CanHavePrefixAspect,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22] = HPathCons[UTEncHPath21[_,_,_,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21],R,A,P,_,_,T22]
  
}