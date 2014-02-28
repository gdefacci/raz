package org.obl.raz

object HPaths {

  type HPath1[+R <: RelativePathAspect,+A <: CanAddAspect,+P <: CanHavePrefixAspect,T1] = HPathCons[HPathNil[_,_,_],R,A,P,T1]
  type HPath2[+R <: RelativePathAspect,+A <: CanAddAspect,+P <: CanHavePrefixAspect,T1, T2] = HPathCons[HPath1[_,_,_,T1],R,A,P,T2]
  type HPath3[+R <: RelativePathAspect,+A <: CanAddAspect,+P <: CanHavePrefixAspect,T1, T2, T3] = HPathCons[HPath2[_,_,_,T1, T2],R,A,P,T3]
  type HPath4[+R <: RelativePathAspect,+A <: CanAddAspect,+P <: CanHavePrefixAspect,T1, T2, T3, T4] = HPathCons[HPath3[_,_,_,T1, T2, T3],R,A,P,T4]
  type HPath5[+R <: RelativePathAspect,+A <: CanAddAspect,+P <: CanHavePrefixAspect,T1, T2, T3, T4, T5] = HPathCons[HPath4[_,_,_,T1, T2, T3, T4],R,A,P,T5]
  type HPath6[+R <: RelativePathAspect,+A <: CanAddAspect,+P <: CanHavePrefixAspect,T1, T2, T3, T4, T5, T6] = HPathCons[HPath5[_,_,_,T1, T2, T3, T4, T5],R,A,P,T6]
  type HPath7[+R <: RelativePathAspect,+A <: CanAddAspect,+P <: CanHavePrefixAspect,T1, T2, T3, T4, T5, T6, T7] = HPathCons[HPath6[_,_,_,T1, T2, T3, T4, T5, T6],R,A,P,T7]
  type HPath8[+R <: RelativePathAspect,+A <: CanAddAspect,+P <: CanHavePrefixAspect,T1, T2, T3, T4, T5, T6, T7, T8] = HPathCons[HPath7[_,_,_,T1, T2, T3, T4, T5, T6, T7],R,A,P,T8]
  type HPath9[+R <: RelativePathAspect,+A <: CanAddAspect,+P <: CanHavePrefixAspect,T1, T2, T3, T4, T5, T6, T7, T8, T9] = HPathCons[HPath8[_,_,_,T1, T2, T3, T4, T5, T6, T7, T8],R,A,P,T9]
  type HPath10[+R <: RelativePathAspect,+A <: CanAddAspect,+P <: CanHavePrefixAspect,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10] = HPathCons[HPath9[_,_,_,T1, T2, T3, T4, T5, T6, T7, T8, T9],R,A,P,T10]
  type HPath11[+R <: RelativePathAspect,+A <: CanAddAspect,+P <: CanHavePrefixAspect,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11] = HPathCons[HPath10[_,_,_,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10],R,A,P,T11]
  type HPath12[+R <: RelativePathAspect,+A <: CanAddAspect,+P <: CanHavePrefixAspect,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12] = HPathCons[HPath11[_,_,_,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11],R,A,P,T12]
  type HPath13[+R <: RelativePathAspect,+A <: CanAddAspect,+P <: CanHavePrefixAspect,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13] = HPathCons[HPath12[_,_,_,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12],R,A,P,T13]
  type HPath14[+R <: RelativePathAspect,+A <: CanAddAspect,+P <: CanHavePrefixAspect,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14] = HPathCons[HPath13[_,_,_,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13],R,A,P,T14]
  type HPath15[+R <: RelativePathAspect,+A <: CanAddAspect,+P <: CanHavePrefixAspect,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15] = HPathCons[HPath14[_,_,_,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14],R,A,P,T15]
  type HPath16[+R <: RelativePathAspect,+A <: CanAddAspect,+P <: CanHavePrefixAspect,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16] = HPathCons[HPath15[_,_,_,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15],R,A,P,T16]
  type HPath17[+R <: RelativePathAspect,+A <: CanAddAspect,+P <: CanHavePrefixAspect,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17] = HPathCons[HPath16[_,_,_,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16],R,A,P,T17]
  type HPath18[+R <: RelativePathAspect,+A <: CanAddAspect,+P <: CanHavePrefixAspect,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18] = HPathCons[HPath17[_,_,_,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17],R,A,P,T18]
  type HPath19[+R <: RelativePathAspect,+A <: CanAddAspect,+P <: CanHavePrefixAspect,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = HPathCons[HPath18[_,_,_,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18],R,A,P,T19]
  type HPath20[+R <: RelativePathAspect,+A <: CanAddAspect,+P <: CanHavePrefixAspect,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20] = HPathCons[HPath19[_,_,_,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19],R,A,P,T20]
  type HPath21[+R <: RelativePathAspect,+A <: CanAddAspect,+P <: CanHavePrefixAspect,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21] = HPathCons[HPath20[_,_,_,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20],R,A,P,T21]
  type HPath22[+R <: RelativePathAspect,+A <: CanAddAspect,+P <: CanHavePrefixAspect,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22] = HPathCons[HPath21[_,_,_,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21],R,A,P,T22]
  
}