package org.obl.raz

object EncHPaths {

  type EncHPath1[+R <: RelativePathAspect,A <: CanAddAspect,+P <: CanHavePrefixAspect,T1] = HPathCons[HPathNil[_,_,_],R,A,P,_,T1,_]
  type EncHPath2[+R <: RelativePathAspect,A <: CanAddAspect,+P <: CanHavePrefixAspect,T1, T2] = HPathCons[EncHPath1[_,_,_,T1],R,A,P,_,T2,_]
  type EncHPath3[+R <: RelativePathAspect,A <: CanAddAspect,+P <: CanHavePrefixAspect,T1, T2, T3] = HPathCons[EncHPath2[_,_,_,T1, T2],R,A,P,_,T3,_]
  type EncHPath4[+R <: RelativePathAspect,A <: CanAddAspect,+P <: CanHavePrefixAspect,T1, T2, T3, T4] = HPathCons[EncHPath3[_,_,_,T1, T2, T3],R,A,P,_,T4,_]
  type EncHPath5[+R <: RelativePathAspect,A <: CanAddAspect,+P <: CanHavePrefixAspect,T1, T2, T3, T4, T5] = HPathCons[EncHPath4[_,_,_,T1, T2, T3, T4],R,A,P,_,T5,_]
  type EncHPath6[+R <: RelativePathAspect,A <: CanAddAspect,+P <: CanHavePrefixAspect,T1, T2, T3, T4, T5, T6] = HPathCons[EncHPath5[_,_,_,T1, T2, T3, T4, T5],R,A,P,_,T6,_]
  type EncHPath7[+R <: RelativePathAspect,A <: CanAddAspect,+P <: CanHavePrefixAspect,T1, T2, T3, T4, T5, T6, T7] = HPathCons[EncHPath6[_,_,_,T1, T2, T3, T4, T5, T6],R,A,P,_,T7,_]
  type EncHPath8[+R <: RelativePathAspect,A <: CanAddAspect,+P <: CanHavePrefixAspect,T1, T2, T3, T4, T5, T6, T7, T8] = HPathCons[EncHPath7[_,_,_,T1, T2, T3, T4, T5, T6, T7],R,A,P,_,T8,_]
  type EncHPath9[+R <: RelativePathAspect,A <: CanAddAspect,+P <: CanHavePrefixAspect,T1, T2, T3, T4, T5, T6, T7, T8, T9] = HPathCons[EncHPath8[_,_,_,T1, T2, T3, T4, T5, T6, T7, T8],R,A,P,_,T9,_]
  type EncHPath10[+R <: RelativePathAspect,A <: CanAddAspect,+P <: CanHavePrefixAspect,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10] = HPathCons[EncHPath9[_,_,_,T1, T2, T3, T4, T5, T6, T7, T8, T9],R,A,P,_,T10,_]
  type EncHPath11[+R <: RelativePathAspect,A <: CanAddAspect,+P <: CanHavePrefixAspect,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11] = HPathCons[EncHPath10[_,_,_,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10],R,A,P,_,T11,_]
  type EncHPath12[+R <: RelativePathAspect,A <: CanAddAspect,+P <: CanHavePrefixAspect,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12] = HPathCons[EncHPath11[_,_,_,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11],R,A,P,_,T12,_]
  type EncHPath13[+R <: RelativePathAspect,A <: CanAddAspect,+P <: CanHavePrefixAspect,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13] = HPathCons[EncHPath12[_,_,_,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12],R,A,P,_,T13,_]
  type EncHPath14[+R <: RelativePathAspect,A <: CanAddAspect,+P <: CanHavePrefixAspect,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14] = HPathCons[EncHPath13[_,_,_,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13],R,A,P,_,T14,_]
  type EncHPath15[+R <: RelativePathAspect,A <: CanAddAspect,+P <: CanHavePrefixAspect,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15] = HPathCons[EncHPath14[_,_,_,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14],R,A,P,_,T15,_]
  type EncHPath16[+R <: RelativePathAspect,A <: CanAddAspect,+P <: CanHavePrefixAspect,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16] = HPathCons[EncHPath15[_,_,_,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15],R,A,P,_,T16,_]
  type EncHPath17[+R <: RelativePathAspect,A <: CanAddAspect,+P <: CanHavePrefixAspect,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17] = HPathCons[EncHPath16[_,_,_,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16],R,A,P,_,T17,_]
  type EncHPath18[+R <: RelativePathAspect,A <: CanAddAspect,+P <: CanHavePrefixAspect,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18] = HPathCons[EncHPath17[_,_,_,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17],R,A,P,_,T18,_]
  type EncHPath19[+R <: RelativePathAspect,A <: CanAddAspect,+P <: CanHavePrefixAspect,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = HPathCons[EncHPath18[_,_,_,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18],R,A,P,_,T19,_]
  type EncHPath20[+R <: RelativePathAspect,A <: CanAddAspect,+P <: CanHavePrefixAspect,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20] = HPathCons[EncHPath19[_,_,_,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19],R,A,P,_,T20,_]
  type EncHPath21[+R <: RelativePathAspect,A <: CanAddAspect,+P <: CanHavePrefixAspect,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21] = HPathCons[EncHPath20[_,_,_,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20],R,A,P,_,T21,_]
  type EncHPath22[+R <: RelativePathAspect,A <: CanAddAspect,+P <: CanHavePrefixAspect,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22] = HPathCons[EncHPath21[_,_,_,T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21],R,A,P,_,T22,_]
  
}