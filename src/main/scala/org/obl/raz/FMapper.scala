package org.obl.raz

trait FMapper[-H, T, +HR[_]] {
  def mapTo[P](cnv: Converter[T, P]): HR[P]
}

object FMapper {

  object TPar {
    def apply[R <: HRoot] = new TPar[R]
  }
  class TPar[R <: HRoot] {
    type Type[+T] = ParamsHResource[R, T]
  }

  type TPath[+T] = PathHResource[RootPath, T]
  type TPathAndPar[+T] = PathAndParamsHResource[RootPath, T]

  import PathHelper._
  import HElems._
  import PathFSum._
  private val emptyPath = Path.empty

  
def matcher1[T1, P](h: HElem1[T1], cnv: (T1) => P): Path => Option[PathMatchResult[P, Path]] = { pth =>
  BasePathMatchers.matcher1[T1](h)(pth).map(r => r.mapValue(cnv))
}

def matcher2[T1,T2, P](h: HElem2[T1,T2], cnv: ((T1,T2)) => P): Path => Option[PathMatchResult[P, Path]] = { pth =>
  BasePathMatchers.matcher2[T1,T2](h)(pth).map(r => r.mapValue(cnv))
}

def matcher3[T1,T2,T3, P](h: HElem3[T1,T2,T3], cnv: ((T1,T2,T3)) => P): Path => Option[PathMatchResult[P, Path]] = { pth =>
  BasePathMatchers.matcher3[T1,T2,T3](h)(pth).map(r => r.mapValue(cnv))
}

def matcher4[T1,T2,T3,T4, P](h: HElem4[T1,T2,T3,T4], cnv: ((T1,T2,T3,T4)) => P): Path => Option[PathMatchResult[P, Path]] = { pth =>
  BasePathMatchers.matcher4[T1,T2,T3,T4](h)(pth).map(r => r.mapValue(cnv))
}

def matcher5[T1,T2,T3,T4,T5, P](h: HElem5[T1,T2,T3,T4,T5], cnv: ((T1,T2,T3,T4,T5)) => P): Path => Option[PathMatchResult[P, Path]] = { pth =>
  BasePathMatchers.matcher5[T1,T2,T3,T4,T5](h)(pth).map(r => r.mapValue(cnv))
}

def matcher6[T1,T2,T3,T4,T5,T6, P](h: HElem6[T1,T2,T3,T4,T5,T6], cnv: ((T1,T2,T3,T4,T5,T6)) => P): Path => Option[PathMatchResult[P, Path]] = { pth =>
  BasePathMatchers.matcher6[T1,T2,T3,T4,T5,T6](h)(pth).map(r => r.mapValue(cnv))
}

def matcher7[T1,T2,T3,T4,T5,T6,T7, P](h: HElem7[T1,T2,T3,T4,T5,T6,T7], cnv: ((T1,T2,T3,T4,T5,T6,T7)) => P): Path => Option[PathMatchResult[P, Path]] = { pth =>
  BasePathMatchers.matcher7[T1,T2,T3,T4,T5,T6,T7](h)(pth).map(r => r.mapValue(cnv))
}

def matcher8[T1,T2,T3,T4,T5,T6,T7,T8, P](h: HElem8[T1,T2,T3,T4,T5,T6,T7,T8], cnv: ((T1,T2,T3,T4,T5,T6,T7,T8)) => P): Path => Option[PathMatchResult[P, Path]] = { pth =>
  BasePathMatchers.matcher8[T1,T2,T3,T4,T5,T6,T7,T8](h)(pth).map(r => r.mapValue(cnv))
}

def matcher9[T1,T2,T3,T4,T5,T6,T7,T8,T9, P](h: HElem9[T1,T2,T3,T4,T5,T6,T7,T8,T9], cnv: ((T1,T2,T3,T4,T5,T6,T7,T8,T9)) => P): Path => Option[PathMatchResult[P, Path]] = { pth =>
  BasePathMatchers.matcher9[T1,T2,T3,T4,T5,T6,T7,T8,T9](h)(pth).map(r => r.mapValue(cnv))
}

def matcher10[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10, P](h: HElem10[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10], cnv: ((T1,T2,T3,T4,T5,T6,T7,T8,T9,T10)) => P): Path => Option[PathMatchResult[P, Path]] = { pth =>
  BasePathMatchers.matcher10[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10](h)(pth).map(r => r.mapValue(cnv))
}

def matcher11[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11, P](h: HElem11[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11], cnv: ((T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11)) => P): Path => Option[PathMatchResult[P, Path]] = { pth =>
  BasePathMatchers.matcher11[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11](h)(pth).map(r => r.mapValue(cnv))
}

def matcher12[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12, P](h: HElem12[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12], cnv: ((T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12)) => P): Path => Option[PathMatchResult[P, Path]] = { pth =>
  BasePathMatchers.matcher12[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12](h)(pth).map(r => r.mapValue(cnv))
}

def matcher13[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13, P](h: HElem13[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13], cnv: ((T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13)) => P): Path => Option[PathMatchResult[P, Path]] = { pth =>
  BasePathMatchers.matcher13[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13](h)(pth).map(r => r.mapValue(cnv))
}

def matcher14[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14, P](h: HElem14[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14], cnv: ((T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14)) => P): Path => Option[PathMatchResult[P, Path]] = { pth =>
  BasePathMatchers.matcher14[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14](h)(pth).map(r => r.mapValue(cnv))
}

def matcher15[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15, P](h: HElem15[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15], cnv: ((T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15)) => P): Path => Option[PathMatchResult[P, Path]] = { pth =>
  BasePathMatchers.matcher15[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15](h)(pth).map(r => r.mapValue(cnv))
}

def matcher16[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16, P](h: HElem16[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16], cnv: ((T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16)) => P): Path => Option[PathMatchResult[P, Path]] = { pth =>
  BasePathMatchers.matcher16[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16](h)(pth).map(r => r.mapValue(cnv))
}

def matcher17[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17, P](h: HElem17[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17], cnv: ((T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17)) => P): Path => Option[PathMatchResult[P, Path]] = { pth =>
  BasePathMatchers.matcher17[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17](h)(pth).map(r => r.mapValue(cnv))
}

def pathFunction1[T1, P](h: HElem1[T1], cnv: P => Option[T1]): P => Path = { p =>
  cnv(p).map(pathFSum1(h)).getOrElse(emptyPath)
}

def pathFunction2[T1,T2, P](h: HElem2[T1,T2], cnv: P => Option[(T1,T2)]): P => Path = { p =>
  cnv(p).map(pathFSum2(h)).getOrElse(emptyPath)
}

def pathFunction3[T1,T2,T3, P](h: HElem3[T1,T2,T3], cnv: P => Option[(T1,T2,T3)]): P => Path = { p =>
  cnv(p).map(pathFSum3(h)).getOrElse(emptyPath)
}

def pathFunction4[T1,T2,T3,T4, P](h: HElem4[T1,T2,T3,T4], cnv: P => Option[(T1,T2,T3,T4)]): P => Path = { p =>
  cnv(p).map(pathFSum4(h)).getOrElse(emptyPath)
}

def pathFunction5[T1,T2,T3,T4,T5, P](h: HElem5[T1,T2,T3,T4,T5], cnv: P => Option[(T1,T2,T3,T4,T5)]): P => Path = { p =>
  cnv(p).map(pathFSum5(h)).getOrElse(emptyPath)
}

def pathFunction6[T1,T2,T3,T4,T5,T6, P](h: HElem6[T1,T2,T3,T4,T5,T6], cnv: P => Option[(T1,T2,T3,T4,T5,T6)]): P => Path = { p =>
  cnv(p).map(pathFSum6(h)).getOrElse(emptyPath)
}

def pathFunction7[T1,T2,T3,T4,T5,T6,T7, P](h: HElem7[T1,T2,T3,T4,T5,T6,T7], cnv: P => Option[(T1,T2,T3,T4,T5,T6,T7)]): P => Path = { p =>
  cnv(p).map(pathFSum7(h)).getOrElse(emptyPath)
}

def pathFunction8[T1,T2,T3,T4,T5,T6,T7,T8, P](h: HElem8[T1,T2,T3,T4,T5,T6,T7,T8], cnv: P => Option[(T1,T2,T3,T4,T5,T6,T7,T8)]): P => Path = { p =>
  cnv(p).map(pathFSum8(h)).getOrElse(emptyPath)
}

def pathFunction9[T1,T2,T3,T4,T5,T6,T7,T8,T9, P](h: HElem9[T1,T2,T3,T4,T5,T6,T7,T8,T9], cnv: P => Option[(T1,T2,T3,T4,T5,T6,T7,T8,T9)]): P => Path = { p =>
  cnv(p).map(pathFSum9(h)).getOrElse(emptyPath)
}

def pathFunction10[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10, P](h: HElem10[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10], cnv: P => Option[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10)]): P => Path = { p =>
  cnv(p).map(pathFSum10(h)).getOrElse(emptyPath)
}

def pathFunction11[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11, P](h: HElem11[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11], cnv: P => Option[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11)]): P => Path = { p =>
  cnv(p).map(pathFSum11(h)).getOrElse(emptyPath)
}

def pathFunction12[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12, P](h: HElem12[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12], cnv: P => Option[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12)]): P => Path = { p =>
  cnv(p).map(pathFSum12(h)).getOrElse(emptyPath)
}

def pathFunction13[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13, P](h: HElem13[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13], cnv: P => Option[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13)]): P => Path = { p =>
  cnv(p).map(pathFSum13(h)).getOrElse(emptyPath)
}

def pathFunction14[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14, P](h: HElem14[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14], cnv: P => Option[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14)]): P => Path = { p =>
  cnv(p).map(pathFSum14(h)).getOrElse(emptyPath)
}

def pathFunction15[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15, P](h: HElem15[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15], cnv: P => Option[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15)]): P => Path = { p =>
  cnv(p).map(pathFSum15(h)).getOrElse(emptyPath)
}

def pathFunction16[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16, P](h: HElem16[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16], cnv: P => Option[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16)]): P => Path = { p =>
  cnv(p).map(pathFSum16(h)).getOrElse(emptyPath)
}

def pathFunction17[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17, P](h: HElem17[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17], cnv: P => Option[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17)]): P => Path = { p =>
  cnv(p).map(pathFSum17(h)).getOrElse(emptyPath)
}

def pathf1[T1,P](h:HElem1[T1], cnv: Converter[T1,P]) = {
  PathF(pathFunction1(h, cnv.unapply), matcher1(h, cnv.apply _), h.value.expansionKind)
}

def pathf2[T1,T2,P](h:HElem2[T1,T2], cnv: Converter[(T1,T2),P]) = {
  PathF(pathFunction2(h, cnv.unapply), matcher2(h, cnv.apply _), h.value.expansionKind)
}

def pathf3[T1,T2,T3,P](h:HElem3[T1,T2,T3], cnv: Converter[(T1,T2,T3),P]) = {
  PathF(pathFunction3(h, cnv.unapply), matcher3(h, cnv.apply _), h.value.expansionKind)
}

def pathf4[T1,T2,T3,T4,P](h:HElem4[T1,T2,T3,T4], cnv: Converter[(T1,T2,T3,T4),P]) = {
  PathF(pathFunction4(h, cnv.unapply), matcher4(h, cnv.apply _), h.value.expansionKind)
}

def pathf5[T1,T2,T3,T4,T5,P](h:HElem5[T1,T2,T3,T4,T5], cnv: Converter[(T1,T2,T3,T4,T5),P]) = {
  PathF(pathFunction5(h, cnv.unapply), matcher5(h, cnv.apply _), h.value.expansionKind)
}

def pathf6[T1,T2,T3,T4,T5,T6,P](h:HElem6[T1,T2,T3,T4,T5,T6], cnv: Converter[(T1,T2,T3,T4,T5,T6),P]) = {
  PathF(pathFunction6(h, cnv.unapply), matcher6(h, cnv.apply _), h.value.expansionKind)
}

def pathf7[T1,T2,T3,T4,T5,T6,T7,P](h:HElem7[T1,T2,T3,T4,T5,T6,T7], cnv: Converter[(T1,T2,T3,T4,T5,T6,T7),P]) = {
  PathF(pathFunction7(h, cnv.unapply), matcher7(h, cnv.apply _), h.value.expansionKind)
}

def pathf8[T1,T2,T3,T4,T5,T6,T7,T8,P](h:HElem8[T1,T2,T3,T4,T5,T6,T7,T8], cnv: Converter[(T1,T2,T3,T4,T5,T6,T7,T8),P]) = {
  PathF(pathFunction8(h, cnv.unapply), matcher8(h, cnv.apply _), h.value.expansionKind)
}

def pathf9[T1,T2,T3,T4,T5,T6,T7,T8,T9,P](h:HElem9[T1,T2,T3,T4,T5,T6,T7,T8,T9], cnv: Converter[(T1,T2,T3,T4,T5,T6,T7,T8,T9),P]) = {
  PathF(pathFunction9(h, cnv.unapply), matcher9(h, cnv.apply _), h.value.expansionKind)
}

def pathf10[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,P](h:HElem10[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10], cnv: Converter[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10),P]) = {
  PathF(pathFunction10(h, cnv.unapply), matcher10(h, cnv.apply _), h.value.expansionKind)
}

def pathf11[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,P](h:HElem11[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11], cnv: Converter[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11),P]) = {
  PathF(pathFunction11(h, cnv.unapply), matcher11(h, cnv.apply _), h.value.expansionKind)
}

def pathf12[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,P](h:HElem12[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12], cnv: Converter[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12),P]) = {
  PathF(pathFunction12(h, cnv.unapply), matcher12(h, cnv.apply _), h.value.expansionKind)
}

def pathf13[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,P](h:HElem13[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13], cnv: Converter[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13),P]) = {
  PathF(pathFunction13(h, cnv.unapply), matcher13(h, cnv.apply _), h.value.expansionKind)
}

def pathf14[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,P](h:HElem14[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14], cnv: Converter[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14),P]) = {
  PathF(pathFunction14(h, cnv.unapply), matcher14(h, cnv.apply _), h.value.expansionKind)
}

def pathf15[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,P](h:HElem15[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15], cnv: Converter[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15),P]) = {
  PathF(pathFunction15(h, cnv.unapply), matcher15(h, cnv.apply _), h.value.expansionKind)
}

def pathf16[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,P](h:HElem16[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16], cnv: Converter[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16),P]) = {
  PathF(pathFunction16(h, cnv.unapply), matcher16(h, cnv.apply _), h.value.expansionKind)
}

def pathf17[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,P](h:HElem17[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17], cnv: Converter[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17),P]) = {
  PathF(pathFunction17(h, cnv.unapply), matcher17(h, cnv.apply _), h.value.expansionKind)
}

implicit def fmapper1a[T1](h:HParamsElems.RootPath.HParamsElem1[T1]) = new FMapper[HParamsElems.RootPath.HParamsElem1[T1], T1, TPathAndPar] {
  def mapTo[P](cnv:Converter[T1,P]):PathAndParamsHResource[RootPath, P] = new PathAndParamsHResource[RootPath,P](h.head, pathf1(h, cnv))
}
  
implicit def fmapper1b[T1](h:HParamsElems.RootUri.HParamsElem1[T1]) = new FMapper[HParamsElems.RootUri.HParamsElem1[T1], T1, TPar[RootUri]#Type] {
  def mapTo[P](cnv:Converter[T1,P]):ParamsHResource[RootUri, P] = new ParamsHResource[RootUri,P](h.head, pathf1(h, cnv))
}
  
implicit def fmapper1c[T1](h:HParamsElems.RootParams.HParamsElem1[T1]) = new FMapper[HParamsElems.RootParams.HParamsElem1[T1], T1, TPar[RootParams]#Type] {
  def mapTo[P](cnv:Converter[T1,P]):ParamsHResource[RootParams, P] = new ParamsHResource[RootParams,P](h.head, pathf1(h, cnv))
}
  
implicit def fmapper1d[T1](h:HPathElems.HPathElem1[T1]) = new FMapper[HPathElems.HPathElem1[T1], T1, TPath] {
  def mapTo[P](cnv:Converter[T1,P]):PathHResource[RootPath, P] = new PathHResource[RootPath,P](h.head, pathf1(h, cnv))
}
  
implicit def fmapper1e[T1](h:HPathAndParamsElems.HPathAndParamsElem1[T1]) = new FMapper[HPathAndParamsElems.HPathAndParamsElem1[T1], T1, TPathAndPar] {
  def mapTo[P](cnv:Converter[T1,P]):PathAndParamsHResource[RootPath, P] = new PathAndParamsHResource[RootPath,P](h.head, pathf1(h, cnv))
}

implicit def fmapper2a[T1,T2](h:HParamsElems.RootPath.HParamsElem2[T1,T2]) = new FMapper[HParamsElems.RootPath.HParamsElem2[T1,T2], (T1,T2), TPathAndPar] {
  def mapTo[P](cnv:Converter[(T1,T2),P]):PathAndParamsHResource[RootPath, P] = new PathAndParamsHResource[RootPath,P](h.head.head, pathf2(h, cnv))
}
  
implicit def fmapper2b[T1,T2](h:HParamsElems.RootUri.HParamsElem2[T1,T2]) = new FMapper[HParamsElems.RootUri.HParamsElem2[T1,T2], (T1,T2), TPar[RootUri]#Type] {
  def mapTo[P](cnv:Converter[(T1,T2),P]):ParamsHResource[RootUri, P] = new ParamsHResource[RootUri,P](h.head.head, pathf2(h, cnv))
}
  
implicit def fmapper2c[T1,T2](h:HParamsElems.RootParams.HParamsElem2[T1,T2]) = new FMapper[HParamsElems.RootParams.HParamsElem2[T1,T2], (T1,T2), TPar[RootParams]#Type] {
  def mapTo[P](cnv:Converter[(T1,T2),P]):ParamsHResource[RootParams, P] = new ParamsHResource[RootParams,P](h.head.head, pathf2(h, cnv))
}
  
implicit def fmapper2d[T1,T2](h:HPathElems.HPathElem2[T1,T2]) = new FMapper[HPathElems.HPathElem2[T1,T2], (T1,T2), TPath] {
  def mapTo[P](cnv:Converter[(T1,T2),P]):PathHResource[RootPath, P] = new PathHResource[RootPath,P](h.head.head, pathf2(h, cnv))
}
  
implicit def fmapper2e[T1,T2](h:HPathAndParamsElems.HPathAndParamsElem2[T1,T2]) = new FMapper[HPathAndParamsElems.HPathAndParamsElem2[T1,T2], (T1,T2), TPathAndPar] {
  def mapTo[P](cnv:Converter[(T1,T2),P]):PathAndParamsHResource[RootPath, P] = new PathAndParamsHResource[RootPath,P](h.head.head, pathf2(h, cnv))
}

implicit def fmapper3a[T1,T2,T3](h:HParamsElems.RootPath.HParamsElem3[T1,T2,T3]) = new FMapper[HParamsElems.RootPath.HParamsElem3[T1,T2,T3], (T1,T2,T3), TPathAndPar] {
  def mapTo[P](cnv:Converter[(T1,T2,T3),P]):PathAndParamsHResource[RootPath, P] = new PathAndParamsHResource[RootPath,P](h.head.head.head, pathf3(h, cnv))
}
  
implicit def fmapper3b[T1,T2,T3](h:HParamsElems.RootUri.HParamsElem3[T1,T2,T3]) = new FMapper[HParamsElems.RootUri.HParamsElem3[T1,T2,T3], (T1,T2,T3), TPar[RootUri]#Type] {
  def mapTo[P](cnv:Converter[(T1,T2,T3),P]):ParamsHResource[RootUri, P] = new ParamsHResource[RootUri,P](h.head.head.head, pathf3(h, cnv))
}
  
implicit def fmapper3c[T1,T2,T3](h:HParamsElems.RootParams.HParamsElem3[T1,T2,T3]) = new FMapper[HParamsElems.RootParams.HParamsElem3[T1,T2,T3], (T1,T2,T3), TPar[RootParams]#Type] {
  def mapTo[P](cnv:Converter[(T1,T2,T3),P]):ParamsHResource[RootParams, P] = new ParamsHResource[RootParams,P](h.head.head.head, pathf3(h, cnv))
}
  
implicit def fmapper3d[T1,T2,T3](h:HPathElems.HPathElem3[T1,T2,T3]) = new FMapper[HPathElems.HPathElem3[T1,T2,T3], (T1,T2,T3), TPath] {
  def mapTo[P](cnv:Converter[(T1,T2,T3),P]):PathHResource[RootPath, P] = new PathHResource[RootPath,P](h.head.head.head, pathf3(h, cnv))
}
  
implicit def fmapper3e[T1,T2,T3](h:HPathAndParamsElems.HPathAndParamsElem3[T1,T2,T3]) = new FMapper[HPathAndParamsElems.HPathAndParamsElem3[T1,T2,T3], (T1,T2,T3), TPathAndPar] {
  def mapTo[P](cnv:Converter[(T1,T2,T3),P]):PathAndParamsHResource[RootPath, P] = new PathAndParamsHResource[RootPath,P](h.head.head.head, pathf3(h, cnv))
}

implicit def fmapper4a[T1,T2,T3,T4](h:HParamsElems.RootPath.HParamsElem4[T1,T2,T3,T4]) = new FMapper[HParamsElems.RootPath.HParamsElem4[T1,T2,T3,T4], (T1,T2,T3,T4), TPathAndPar] {
  def mapTo[P](cnv:Converter[(T1,T2,T3,T4),P]):PathAndParamsHResource[RootPath, P] = new PathAndParamsHResource[RootPath,P](h.head.head.head.head, pathf4(h, cnv))
}
  
implicit def fmapper4b[T1,T2,T3,T4](h:HParamsElems.RootUri.HParamsElem4[T1,T2,T3,T4]) = new FMapper[HParamsElems.RootUri.HParamsElem4[T1,T2,T3,T4], (T1,T2,T3,T4), TPar[RootUri]#Type] {
  def mapTo[P](cnv:Converter[(T1,T2,T3,T4),P]):ParamsHResource[RootUri, P] = new ParamsHResource[RootUri,P](h.head.head.head.head, pathf4(h, cnv))
}
  
implicit def fmapper4c[T1,T2,T3,T4](h:HParamsElems.RootParams.HParamsElem4[T1,T2,T3,T4]) = new FMapper[HParamsElems.RootParams.HParamsElem4[T1,T2,T3,T4], (T1,T2,T3,T4), TPar[RootParams]#Type] {
  def mapTo[P](cnv:Converter[(T1,T2,T3,T4),P]):ParamsHResource[RootParams, P] = new ParamsHResource[RootParams,P](h.head.head.head.head, pathf4(h, cnv))
}
  
implicit def fmapper4d[T1,T2,T3,T4](h:HPathElems.HPathElem4[T1,T2,T3,T4]) = new FMapper[HPathElems.HPathElem4[T1,T2,T3,T4], (T1,T2,T3,T4), TPath] {
  def mapTo[P](cnv:Converter[(T1,T2,T3,T4),P]):PathHResource[RootPath, P] = new PathHResource[RootPath,P](h.head.head.head.head, pathf4(h, cnv))
}
  
implicit def fmapper4e[T1,T2,T3,T4](h:HPathAndParamsElems.HPathAndParamsElem4[T1,T2,T3,T4]) = new FMapper[HPathAndParamsElems.HPathAndParamsElem4[T1,T2,T3,T4], (T1,T2,T3,T4), TPathAndPar] {
  def mapTo[P](cnv:Converter[(T1,T2,T3,T4),P]):PathAndParamsHResource[RootPath, P] = new PathAndParamsHResource[RootPath,P](h.head.head.head.head, pathf4(h, cnv))
}

implicit def fmapper5a[T1,T2,T3,T4,T5](h:HParamsElems.RootPath.HParamsElem5[T1,T2,T3,T4,T5]) = new FMapper[HParamsElems.RootPath.HParamsElem5[T1,T2,T3,T4,T5], (T1,T2,T3,T4,T5), TPathAndPar] {
  def mapTo[P](cnv:Converter[(T1,T2,T3,T4,T5),P]):PathAndParamsHResource[RootPath, P] = new PathAndParamsHResource[RootPath,P](h.head.head.head.head.head, pathf5(h, cnv))
}
  
implicit def fmapper5b[T1,T2,T3,T4,T5](h:HParamsElems.RootUri.HParamsElem5[T1,T2,T3,T4,T5]) = new FMapper[HParamsElems.RootUri.HParamsElem5[T1,T2,T3,T4,T5], (T1,T2,T3,T4,T5), TPar[RootUri]#Type] {
  def mapTo[P](cnv:Converter[(T1,T2,T3,T4,T5),P]):ParamsHResource[RootUri, P] = new ParamsHResource[RootUri,P](h.head.head.head.head.head, pathf5(h, cnv))
}
  
implicit def fmapper5c[T1,T2,T3,T4,T5](h:HParamsElems.RootParams.HParamsElem5[T1,T2,T3,T4,T5]) = new FMapper[HParamsElems.RootParams.HParamsElem5[T1,T2,T3,T4,T5], (T1,T2,T3,T4,T5), TPar[RootParams]#Type] {
  def mapTo[P](cnv:Converter[(T1,T2,T3,T4,T5),P]):ParamsHResource[RootParams, P] = new ParamsHResource[RootParams,P](h.head.head.head.head.head, pathf5(h, cnv))
}
  
implicit def fmapper5d[T1,T2,T3,T4,T5](h:HPathElems.HPathElem5[T1,T2,T3,T4,T5]) = new FMapper[HPathElems.HPathElem5[T1,T2,T3,T4,T5], (T1,T2,T3,T4,T5), TPath] {
  def mapTo[P](cnv:Converter[(T1,T2,T3,T4,T5),P]):PathHResource[RootPath, P] = new PathHResource[RootPath,P](h.head.head.head.head.head, pathf5(h, cnv))
}
  
implicit def fmapper5e[T1,T2,T3,T4,T5](h:HPathAndParamsElems.HPathAndParamsElem5[T1,T2,T3,T4,T5]) = new FMapper[HPathAndParamsElems.HPathAndParamsElem5[T1,T2,T3,T4,T5], (T1,T2,T3,T4,T5), TPathAndPar] {
  def mapTo[P](cnv:Converter[(T1,T2,T3,T4,T5),P]):PathAndParamsHResource[RootPath, P] = new PathAndParamsHResource[RootPath,P](h.head.head.head.head.head, pathf5(h, cnv))
}

implicit def fmapper6a[T1,T2,T3,T4,T5,T6](h:HParamsElems.RootPath.HParamsElem6[T1,T2,T3,T4,T5,T6]) = new FMapper[HParamsElems.RootPath.HParamsElem6[T1,T2,T3,T4,T5,T6], (T1,T2,T3,T4,T5,T6), TPathAndPar] {
  def mapTo[P](cnv:Converter[(T1,T2,T3,T4,T5,T6),P]):PathAndParamsHResource[RootPath, P] = new PathAndParamsHResource[RootPath,P](h.head.head.head.head.head.head, pathf6(h, cnv))
}
  
implicit def fmapper6b[T1,T2,T3,T4,T5,T6](h:HParamsElems.RootUri.HParamsElem6[T1,T2,T3,T4,T5,T6]) = new FMapper[HParamsElems.RootUri.HParamsElem6[T1,T2,T3,T4,T5,T6], (T1,T2,T3,T4,T5,T6), TPar[RootUri]#Type] {
  def mapTo[P](cnv:Converter[(T1,T2,T3,T4,T5,T6),P]):ParamsHResource[RootUri, P] = new ParamsHResource[RootUri,P](h.head.head.head.head.head.head, pathf6(h, cnv))
}
  
implicit def fmapper6c[T1,T2,T3,T4,T5,T6](h:HParamsElems.RootParams.HParamsElem6[T1,T2,T3,T4,T5,T6]) = new FMapper[HParamsElems.RootParams.HParamsElem6[T1,T2,T3,T4,T5,T6], (T1,T2,T3,T4,T5,T6), TPar[RootParams]#Type] {
  def mapTo[P](cnv:Converter[(T1,T2,T3,T4,T5,T6),P]):ParamsHResource[RootParams, P] = new ParamsHResource[RootParams,P](h.head.head.head.head.head.head, pathf6(h, cnv))
}
  
implicit def fmapper6d[T1,T2,T3,T4,T5,T6](h:HPathElems.HPathElem6[T1,T2,T3,T4,T5,T6]) = new FMapper[HPathElems.HPathElem6[T1,T2,T3,T4,T5,T6], (T1,T2,T3,T4,T5,T6), TPath] {
  def mapTo[P](cnv:Converter[(T1,T2,T3,T4,T5,T6),P]):PathHResource[RootPath, P] = new PathHResource[RootPath,P](h.head.head.head.head.head.head, pathf6(h, cnv))
}
  
implicit def fmapper6e[T1,T2,T3,T4,T5,T6](h:HPathAndParamsElems.HPathAndParamsElem6[T1,T2,T3,T4,T5,T6]) = new FMapper[HPathAndParamsElems.HPathAndParamsElem6[T1,T2,T3,T4,T5,T6], (T1,T2,T3,T4,T5,T6), TPathAndPar] {
  def mapTo[P](cnv:Converter[(T1,T2,T3,T4,T5,T6),P]):PathAndParamsHResource[RootPath, P] = new PathAndParamsHResource[RootPath,P](h.head.head.head.head.head.head, pathf6(h, cnv))
}

implicit def fmapper7a[T1,T2,T3,T4,T5,T6,T7](h:HParamsElems.RootPath.HParamsElem7[T1,T2,T3,T4,T5,T6,T7]) = new FMapper[HParamsElems.RootPath.HParamsElem7[T1,T2,T3,T4,T5,T6,T7], (T1,T2,T3,T4,T5,T6,T7), TPathAndPar] {
  def mapTo[P](cnv:Converter[(T1,T2,T3,T4,T5,T6,T7),P]):PathAndParamsHResource[RootPath, P] = new PathAndParamsHResource[RootPath,P](h.head.head.head.head.head.head.head, pathf7(h, cnv))
}
  
implicit def fmapper7b[T1,T2,T3,T4,T5,T6,T7](h:HParamsElems.RootUri.HParamsElem7[T1,T2,T3,T4,T5,T6,T7]) = new FMapper[HParamsElems.RootUri.HParamsElem7[T1,T2,T3,T4,T5,T6,T7], (T1,T2,T3,T4,T5,T6,T7), TPar[RootUri]#Type] {
  def mapTo[P](cnv:Converter[(T1,T2,T3,T4,T5,T6,T7),P]):ParamsHResource[RootUri, P] = new ParamsHResource[RootUri,P](h.head.head.head.head.head.head.head, pathf7(h, cnv))
}
  
implicit def fmapper7c[T1,T2,T3,T4,T5,T6,T7](h:HParamsElems.RootParams.HParamsElem7[T1,T2,T3,T4,T5,T6,T7]) = new FMapper[HParamsElems.RootParams.HParamsElem7[T1,T2,T3,T4,T5,T6,T7], (T1,T2,T3,T4,T5,T6,T7), TPar[RootParams]#Type] {
  def mapTo[P](cnv:Converter[(T1,T2,T3,T4,T5,T6,T7),P]):ParamsHResource[RootParams, P] = new ParamsHResource[RootParams,P](h.head.head.head.head.head.head.head, pathf7(h, cnv))
}
  
implicit def fmapper7d[T1,T2,T3,T4,T5,T6,T7](h:HPathElems.HPathElem7[T1,T2,T3,T4,T5,T6,T7]) = new FMapper[HPathElems.HPathElem7[T1,T2,T3,T4,T5,T6,T7], (T1,T2,T3,T4,T5,T6,T7), TPath] {
  def mapTo[P](cnv:Converter[(T1,T2,T3,T4,T5,T6,T7),P]):PathHResource[RootPath, P] = new PathHResource[RootPath,P](h.head.head.head.head.head.head.head, pathf7(h, cnv))
}
  
implicit def fmapper7e[T1,T2,T3,T4,T5,T6,T7](h:HPathAndParamsElems.HPathAndParamsElem7[T1,T2,T3,T4,T5,T6,T7]) = new FMapper[HPathAndParamsElems.HPathAndParamsElem7[T1,T2,T3,T4,T5,T6,T7], (T1,T2,T3,T4,T5,T6,T7), TPathAndPar] {
  def mapTo[P](cnv:Converter[(T1,T2,T3,T4,T5,T6,T7),P]):PathAndParamsHResource[RootPath, P] = new PathAndParamsHResource[RootPath,P](h.head.head.head.head.head.head.head, pathf7(h, cnv))
}

implicit def fmapper8a[T1,T2,T3,T4,T5,T6,T7,T8](h:HParamsElems.RootPath.HParamsElem8[T1,T2,T3,T4,T5,T6,T7,T8]) = new FMapper[HParamsElems.RootPath.HParamsElem8[T1,T2,T3,T4,T5,T6,T7,T8], (T1,T2,T3,T4,T5,T6,T7,T8), TPathAndPar] {
  def mapTo[P](cnv:Converter[(T1,T2,T3,T4,T5,T6,T7,T8),P]):PathAndParamsHResource[RootPath, P] = new PathAndParamsHResource[RootPath,P](h.head.head.head.head.head.head.head.head, pathf8(h, cnv))
}
  
implicit def fmapper8b[T1,T2,T3,T4,T5,T6,T7,T8](h:HParamsElems.RootUri.HParamsElem8[T1,T2,T3,T4,T5,T6,T7,T8]) = new FMapper[HParamsElems.RootUri.HParamsElem8[T1,T2,T3,T4,T5,T6,T7,T8], (T1,T2,T3,T4,T5,T6,T7,T8), TPar[RootUri]#Type] {
  def mapTo[P](cnv:Converter[(T1,T2,T3,T4,T5,T6,T7,T8),P]):ParamsHResource[RootUri, P] = new ParamsHResource[RootUri,P](h.head.head.head.head.head.head.head.head, pathf8(h, cnv))
}
  
implicit def fmapper8c[T1,T2,T3,T4,T5,T6,T7,T8](h:HParamsElems.RootParams.HParamsElem8[T1,T2,T3,T4,T5,T6,T7,T8]) = new FMapper[HParamsElems.RootParams.HParamsElem8[T1,T2,T3,T4,T5,T6,T7,T8], (T1,T2,T3,T4,T5,T6,T7,T8), TPar[RootParams]#Type] {
  def mapTo[P](cnv:Converter[(T1,T2,T3,T4,T5,T6,T7,T8),P]):ParamsHResource[RootParams, P] = new ParamsHResource[RootParams,P](h.head.head.head.head.head.head.head.head, pathf8(h, cnv))
}
  
implicit def fmapper8d[T1,T2,T3,T4,T5,T6,T7,T8](h:HPathElems.HPathElem8[T1,T2,T3,T4,T5,T6,T7,T8]) = new FMapper[HPathElems.HPathElem8[T1,T2,T3,T4,T5,T6,T7,T8], (T1,T2,T3,T4,T5,T6,T7,T8), TPath] {
  def mapTo[P](cnv:Converter[(T1,T2,T3,T4,T5,T6,T7,T8),P]):PathHResource[RootPath, P] = new PathHResource[RootPath,P](h.head.head.head.head.head.head.head.head, pathf8(h, cnv))
}
  
implicit def fmapper8e[T1,T2,T3,T4,T5,T6,T7,T8](h:HPathAndParamsElems.HPathAndParamsElem8[T1,T2,T3,T4,T5,T6,T7,T8]) = new FMapper[HPathAndParamsElems.HPathAndParamsElem8[T1,T2,T3,T4,T5,T6,T7,T8], (T1,T2,T3,T4,T5,T6,T7,T8), TPathAndPar] {
  def mapTo[P](cnv:Converter[(T1,T2,T3,T4,T5,T6,T7,T8),P]):PathAndParamsHResource[RootPath, P] = new PathAndParamsHResource[RootPath,P](h.head.head.head.head.head.head.head.head, pathf8(h, cnv))
}

implicit def fmapper9a[T1,T2,T3,T4,T5,T6,T7,T8,T9](h:HParamsElems.RootPath.HParamsElem9[T1,T2,T3,T4,T5,T6,T7,T8,T9]) = new FMapper[HParamsElems.RootPath.HParamsElem9[T1,T2,T3,T4,T5,T6,T7,T8,T9], (T1,T2,T3,T4,T5,T6,T7,T8,T9), TPathAndPar] {
  def mapTo[P](cnv:Converter[(T1,T2,T3,T4,T5,T6,T7,T8,T9),P]):PathAndParamsHResource[RootPath, P] = new PathAndParamsHResource[RootPath,P](h.head.head.head.head.head.head.head.head.head, pathf9(h, cnv))
}
  
implicit def fmapper9b[T1,T2,T3,T4,T5,T6,T7,T8,T9](h:HParamsElems.RootUri.HParamsElem9[T1,T2,T3,T4,T5,T6,T7,T8,T9]) = new FMapper[HParamsElems.RootUri.HParamsElem9[T1,T2,T3,T4,T5,T6,T7,T8,T9], (T1,T2,T3,T4,T5,T6,T7,T8,T9), TPar[RootUri]#Type] {
  def mapTo[P](cnv:Converter[(T1,T2,T3,T4,T5,T6,T7,T8,T9),P]):ParamsHResource[RootUri, P] = new ParamsHResource[RootUri,P](h.head.head.head.head.head.head.head.head.head, pathf9(h, cnv))
}
  
implicit def fmapper9c[T1,T2,T3,T4,T5,T6,T7,T8,T9](h:HParamsElems.RootParams.HParamsElem9[T1,T2,T3,T4,T5,T6,T7,T8,T9]) = new FMapper[HParamsElems.RootParams.HParamsElem9[T1,T2,T3,T4,T5,T6,T7,T8,T9], (T1,T2,T3,T4,T5,T6,T7,T8,T9), TPar[RootParams]#Type] {
  def mapTo[P](cnv:Converter[(T1,T2,T3,T4,T5,T6,T7,T8,T9),P]):ParamsHResource[RootParams, P] = new ParamsHResource[RootParams,P](h.head.head.head.head.head.head.head.head.head, pathf9(h, cnv))
}
  
implicit def fmapper9d[T1,T2,T3,T4,T5,T6,T7,T8,T9](h:HPathElems.HPathElem9[T1,T2,T3,T4,T5,T6,T7,T8,T9]) = new FMapper[HPathElems.HPathElem9[T1,T2,T3,T4,T5,T6,T7,T8,T9], (T1,T2,T3,T4,T5,T6,T7,T8,T9), TPath] {
  def mapTo[P](cnv:Converter[(T1,T2,T3,T4,T5,T6,T7,T8,T9),P]):PathHResource[RootPath, P] = new PathHResource[RootPath,P](h.head.head.head.head.head.head.head.head.head, pathf9(h, cnv))
}
  
implicit def fmapper9e[T1,T2,T3,T4,T5,T6,T7,T8,T9](h:HPathAndParamsElems.HPathAndParamsElem9[T1,T2,T3,T4,T5,T6,T7,T8,T9]) = new FMapper[HPathAndParamsElems.HPathAndParamsElem9[T1,T2,T3,T4,T5,T6,T7,T8,T9], (T1,T2,T3,T4,T5,T6,T7,T8,T9), TPathAndPar] {
  def mapTo[P](cnv:Converter[(T1,T2,T3,T4,T5,T6,T7,T8,T9),P]):PathAndParamsHResource[RootPath, P] = new PathAndParamsHResource[RootPath,P](h.head.head.head.head.head.head.head.head.head, pathf9(h, cnv))
}

implicit def fmapper10a[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10](h:HParamsElems.RootPath.HParamsElem10[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10]) = new FMapper[HParamsElems.RootPath.HParamsElem10[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10), TPathAndPar] {
  def mapTo[P](cnv:Converter[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10),P]):PathAndParamsHResource[RootPath, P] = new PathAndParamsHResource[RootPath,P](h.head.head.head.head.head.head.head.head.head.head, pathf10(h, cnv))
}
  
implicit def fmapper10b[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10](h:HParamsElems.RootUri.HParamsElem10[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10]) = new FMapper[HParamsElems.RootUri.HParamsElem10[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10), TPar[RootUri]#Type] {
  def mapTo[P](cnv:Converter[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10),P]):ParamsHResource[RootUri, P] = new ParamsHResource[RootUri,P](h.head.head.head.head.head.head.head.head.head.head, pathf10(h, cnv))
}
  
implicit def fmapper10c[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10](h:HParamsElems.RootParams.HParamsElem10[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10]) = new FMapper[HParamsElems.RootParams.HParamsElem10[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10), TPar[RootParams]#Type] {
  def mapTo[P](cnv:Converter[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10),P]):ParamsHResource[RootParams, P] = new ParamsHResource[RootParams,P](h.head.head.head.head.head.head.head.head.head.head, pathf10(h, cnv))
}
  
implicit def fmapper10d[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10](h:HPathElems.HPathElem10[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10]) = new FMapper[HPathElems.HPathElem10[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10), TPath] {
  def mapTo[P](cnv:Converter[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10),P]):PathHResource[RootPath, P] = new PathHResource[RootPath,P](h.head.head.head.head.head.head.head.head.head.head, pathf10(h, cnv))
}
  
implicit def fmapper10e[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10](h:HPathAndParamsElems.HPathAndParamsElem10[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10]) = new FMapper[HPathAndParamsElems.HPathAndParamsElem10[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10), TPathAndPar] {
  def mapTo[P](cnv:Converter[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10),P]):PathAndParamsHResource[RootPath, P] = new PathAndParamsHResource[RootPath,P](h.head.head.head.head.head.head.head.head.head.head, pathf10(h, cnv))
}

implicit def fmapper11a[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11](h:HParamsElems.RootPath.HParamsElem11[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11]) = new FMapper[HParamsElems.RootPath.HParamsElem11[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11), TPathAndPar] {
  def mapTo[P](cnv:Converter[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11),P]):PathAndParamsHResource[RootPath, P] = new PathAndParamsHResource[RootPath,P](h.head.head.head.head.head.head.head.head.head.head.head, pathf11(h, cnv))
}
  
implicit def fmapper11b[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11](h:HParamsElems.RootUri.HParamsElem11[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11]) = new FMapper[HParamsElems.RootUri.HParamsElem11[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11), TPar[RootUri]#Type] {
  def mapTo[P](cnv:Converter[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11),P]):ParamsHResource[RootUri, P] = new ParamsHResource[RootUri,P](h.head.head.head.head.head.head.head.head.head.head.head, pathf11(h, cnv))
}
  
implicit def fmapper11c[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11](h:HParamsElems.RootParams.HParamsElem11[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11]) = new FMapper[HParamsElems.RootParams.HParamsElem11[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11), TPar[RootParams]#Type] {
  def mapTo[P](cnv:Converter[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11),P]):ParamsHResource[RootParams, P] = new ParamsHResource[RootParams,P](h.head.head.head.head.head.head.head.head.head.head.head, pathf11(h, cnv))
}
  
implicit def fmapper11d[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11](h:HPathElems.HPathElem11[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11]) = new FMapper[HPathElems.HPathElem11[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11), TPath] {
  def mapTo[P](cnv:Converter[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11),P]):PathHResource[RootPath, P] = new PathHResource[RootPath,P](h.head.head.head.head.head.head.head.head.head.head.head, pathf11(h, cnv))
}
  
implicit def fmapper11e[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11](h:HPathAndParamsElems.HPathAndParamsElem11[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11]) = new FMapper[HPathAndParamsElems.HPathAndParamsElem11[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11), TPathAndPar] {
  def mapTo[P](cnv:Converter[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11),P]):PathAndParamsHResource[RootPath, P] = new PathAndParamsHResource[RootPath,P](h.head.head.head.head.head.head.head.head.head.head.head, pathf11(h, cnv))
}

implicit def fmapper12a[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12](h:HParamsElems.RootPath.HParamsElem12[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12]) = new FMapper[HParamsElems.RootPath.HParamsElem12[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12), TPathAndPar] {
  def mapTo[P](cnv:Converter[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12),P]):PathAndParamsHResource[RootPath, P] = new PathAndParamsHResource[RootPath,P](h.head.head.head.head.head.head.head.head.head.head.head.head, pathf12(h, cnv))
}
  
implicit def fmapper12b[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12](h:HParamsElems.RootUri.HParamsElem12[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12]) = new FMapper[HParamsElems.RootUri.HParamsElem12[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12), TPar[RootUri]#Type] {
  def mapTo[P](cnv:Converter[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12),P]):ParamsHResource[RootUri, P] = new ParamsHResource[RootUri,P](h.head.head.head.head.head.head.head.head.head.head.head.head, pathf12(h, cnv))
}
  
implicit def fmapper12c[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12](h:HParamsElems.RootParams.HParamsElem12[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12]) = new FMapper[HParamsElems.RootParams.HParamsElem12[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12), TPar[RootParams]#Type] {
  def mapTo[P](cnv:Converter[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12),P]):ParamsHResource[RootParams, P] = new ParamsHResource[RootParams,P](h.head.head.head.head.head.head.head.head.head.head.head.head, pathf12(h, cnv))
}
  
implicit def fmapper12d[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12](h:HPathElems.HPathElem12[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12]) = new FMapper[HPathElems.HPathElem12[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12), TPath] {
  def mapTo[P](cnv:Converter[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12),P]):PathHResource[RootPath, P] = new PathHResource[RootPath,P](h.head.head.head.head.head.head.head.head.head.head.head.head, pathf12(h, cnv))
}
  
implicit def fmapper12e[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12](h:HPathAndParamsElems.HPathAndParamsElem12[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12]) = new FMapper[HPathAndParamsElems.HPathAndParamsElem12[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12), TPathAndPar] {
  def mapTo[P](cnv:Converter[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12),P]):PathAndParamsHResource[RootPath, P] = new PathAndParamsHResource[RootPath,P](h.head.head.head.head.head.head.head.head.head.head.head.head, pathf12(h, cnv))
}

implicit def fmapper13a[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13](h:HParamsElems.RootPath.HParamsElem13[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13]) = new FMapper[HParamsElems.RootPath.HParamsElem13[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13), TPathAndPar] {
  def mapTo[P](cnv:Converter[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13),P]):PathAndParamsHResource[RootPath, P] = new PathAndParamsHResource[RootPath,P](h.head.head.head.head.head.head.head.head.head.head.head.head.head, pathf13(h, cnv))
}
  
implicit def fmapper13b[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13](h:HParamsElems.RootUri.HParamsElem13[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13]) = new FMapper[HParamsElems.RootUri.HParamsElem13[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13), TPar[RootUri]#Type] {
  def mapTo[P](cnv:Converter[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13),P]):ParamsHResource[RootUri, P] = new ParamsHResource[RootUri,P](h.head.head.head.head.head.head.head.head.head.head.head.head.head, pathf13(h, cnv))
}
  
implicit def fmapper13c[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13](h:HParamsElems.RootParams.HParamsElem13[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13]) = new FMapper[HParamsElems.RootParams.HParamsElem13[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13), TPar[RootParams]#Type] {
  def mapTo[P](cnv:Converter[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13),P]):ParamsHResource[RootParams, P] = new ParamsHResource[RootParams,P](h.head.head.head.head.head.head.head.head.head.head.head.head.head, pathf13(h, cnv))
}
  
implicit def fmapper13d[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13](h:HPathElems.HPathElem13[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13]) = new FMapper[HPathElems.HPathElem13[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13), TPath] {
  def mapTo[P](cnv:Converter[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13),P]):PathHResource[RootPath, P] = new PathHResource[RootPath,P](h.head.head.head.head.head.head.head.head.head.head.head.head.head, pathf13(h, cnv))
}
  
implicit def fmapper13e[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13](h:HPathAndParamsElems.HPathAndParamsElem13[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13]) = new FMapper[HPathAndParamsElems.HPathAndParamsElem13[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13), TPathAndPar] {
  def mapTo[P](cnv:Converter[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13),P]):PathAndParamsHResource[RootPath, P] = new PathAndParamsHResource[RootPath,P](h.head.head.head.head.head.head.head.head.head.head.head.head.head, pathf13(h, cnv))
}

implicit def fmapper14a[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14](h:HParamsElems.RootPath.HParamsElem14[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14]) = new FMapper[HParamsElems.RootPath.HParamsElem14[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14), TPathAndPar] {
  def mapTo[P](cnv:Converter[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14),P]):PathAndParamsHResource[RootPath, P] = new PathAndParamsHResource[RootPath,P](h.head.head.head.head.head.head.head.head.head.head.head.head.head.head, pathf14(h, cnv))
}
  
implicit def fmapper14b[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14](h:HParamsElems.RootUri.HParamsElem14[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14]) = new FMapper[HParamsElems.RootUri.HParamsElem14[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14), TPar[RootUri]#Type] {
  def mapTo[P](cnv:Converter[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14),P]):ParamsHResource[RootUri, P] = new ParamsHResource[RootUri,P](h.head.head.head.head.head.head.head.head.head.head.head.head.head.head, pathf14(h, cnv))
}
  
implicit def fmapper14c[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14](h:HParamsElems.RootParams.HParamsElem14[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14]) = new FMapper[HParamsElems.RootParams.HParamsElem14[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14), TPar[RootParams]#Type] {
  def mapTo[P](cnv:Converter[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14),P]):ParamsHResource[RootParams, P] = new ParamsHResource[RootParams,P](h.head.head.head.head.head.head.head.head.head.head.head.head.head.head, pathf14(h, cnv))
}
  
implicit def fmapper14d[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14](h:HPathElems.HPathElem14[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14]) = new FMapper[HPathElems.HPathElem14[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14), TPath] {
  def mapTo[P](cnv:Converter[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14),P]):PathHResource[RootPath, P] = new PathHResource[RootPath,P](h.head.head.head.head.head.head.head.head.head.head.head.head.head.head, pathf14(h, cnv))
}
  
implicit def fmapper14e[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14](h:HPathAndParamsElems.HPathAndParamsElem14[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14]) = new FMapper[HPathAndParamsElems.HPathAndParamsElem14[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14), TPathAndPar] {
  def mapTo[P](cnv:Converter[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14),P]):PathAndParamsHResource[RootPath, P] = new PathAndParamsHResource[RootPath,P](h.head.head.head.head.head.head.head.head.head.head.head.head.head.head, pathf14(h, cnv))
}

implicit def fmapper15a[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15](h:HParamsElems.RootPath.HParamsElem15[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15]) = new FMapper[HParamsElems.RootPath.HParamsElem15[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15), TPathAndPar] {
  def mapTo[P](cnv:Converter[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15),P]):PathAndParamsHResource[RootPath, P] = new PathAndParamsHResource[RootPath,P](h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head, pathf15(h, cnv))
}
  
implicit def fmapper15b[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15](h:HParamsElems.RootUri.HParamsElem15[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15]) = new FMapper[HParamsElems.RootUri.HParamsElem15[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15), TPar[RootUri]#Type] {
  def mapTo[P](cnv:Converter[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15),P]):ParamsHResource[RootUri, P] = new ParamsHResource[RootUri,P](h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head, pathf15(h, cnv))
}
  
implicit def fmapper15c[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15](h:HParamsElems.RootParams.HParamsElem15[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15]) = new FMapper[HParamsElems.RootParams.HParamsElem15[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15), TPar[RootParams]#Type] {
  def mapTo[P](cnv:Converter[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15),P]):ParamsHResource[RootParams, P] = new ParamsHResource[RootParams,P](h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head, pathf15(h, cnv))
}
  
implicit def fmapper15d[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15](h:HPathElems.HPathElem15[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15]) = new FMapper[HPathElems.HPathElem15[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15), TPath] {
  def mapTo[P](cnv:Converter[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15),P]):PathHResource[RootPath, P] = new PathHResource[RootPath,P](h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head, pathf15(h, cnv))
}
  
implicit def fmapper15e[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15](h:HPathAndParamsElems.HPathAndParamsElem15[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15]) = new FMapper[HPathAndParamsElems.HPathAndParamsElem15[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15), TPathAndPar] {
  def mapTo[P](cnv:Converter[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15),P]):PathAndParamsHResource[RootPath, P] = new PathAndParamsHResource[RootPath,P](h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head, pathf15(h, cnv))
}

implicit def fmapper16a[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16](h:HParamsElems.RootPath.HParamsElem16[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16]) = new FMapper[HParamsElems.RootPath.HParamsElem16[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16), TPathAndPar] {
  def mapTo[P](cnv:Converter[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16),P]):PathAndParamsHResource[RootPath, P] = new PathAndParamsHResource[RootPath,P](h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head, pathf16(h, cnv))
}
  
implicit def fmapper16b[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16](h:HParamsElems.RootUri.HParamsElem16[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16]) = new FMapper[HParamsElems.RootUri.HParamsElem16[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16), TPar[RootUri]#Type] {
  def mapTo[P](cnv:Converter[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16),P]):ParamsHResource[RootUri, P] = new ParamsHResource[RootUri,P](h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head, pathf16(h, cnv))
}
  
implicit def fmapper16c[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16](h:HParamsElems.RootParams.HParamsElem16[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16]) = new FMapper[HParamsElems.RootParams.HParamsElem16[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16), TPar[RootParams]#Type] {
  def mapTo[P](cnv:Converter[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16),P]):ParamsHResource[RootParams, P] = new ParamsHResource[RootParams,P](h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head, pathf16(h, cnv))
}
  
implicit def fmapper16d[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16](h:HPathElems.HPathElem16[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16]) = new FMapper[HPathElems.HPathElem16[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16), TPath] {
  def mapTo[P](cnv:Converter[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16),P]):PathHResource[RootPath, P] = new PathHResource[RootPath,P](h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head, pathf16(h, cnv))
}
  
implicit def fmapper16e[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16](h:HPathAndParamsElems.HPathAndParamsElem16[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16]) = new FMapper[HPathAndParamsElems.HPathAndParamsElem16[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16), TPathAndPar] {
  def mapTo[P](cnv:Converter[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16),P]):PathAndParamsHResource[RootPath, P] = new PathAndParamsHResource[RootPath,P](h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head, pathf16(h, cnv))
}

implicit def fmapper17a[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17](h:HParamsElems.RootPath.HParamsElem17[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17]) = new FMapper[HParamsElems.RootPath.HParamsElem17[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17), TPathAndPar] {
  def mapTo[P](cnv:Converter[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17),P]):PathAndParamsHResource[RootPath, P] = new PathAndParamsHResource[RootPath,P](h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head, pathf17(h, cnv))
}
  
implicit def fmapper17b[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17](h:HParamsElems.RootUri.HParamsElem17[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17]) = new FMapper[HParamsElems.RootUri.HParamsElem17[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17), TPar[RootUri]#Type] {
  def mapTo[P](cnv:Converter[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17),P]):ParamsHResource[RootUri, P] = new ParamsHResource[RootUri,P](h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head, pathf17(h, cnv))
}
  
implicit def fmapper17c[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17](h:HParamsElems.RootParams.HParamsElem17[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17]) = new FMapper[HParamsElems.RootParams.HParamsElem17[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17), TPar[RootParams]#Type] {
  def mapTo[P](cnv:Converter[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17),P]):ParamsHResource[RootParams, P] = new ParamsHResource[RootParams,P](h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head, pathf17(h, cnv))
}
  
implicit def fmapper17d[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17](h:HPathElems.HPathElem17[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17]) = new FMapper[HPathElems.HPathElem17[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17), TPath] {
  def mapTo[P](cnv:Converter[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17),P]):PathHResource[RootPath, P] = new PathHResource[RootPath,P](h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head, pathf17(h, cnv))
}
  
implicit def fmapper17e[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17](h:HPathAndParamsElems.HPathAndParamsElem17[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17]) = new FMapper[HPathAndParamsElems.HPathAndParamsElem17[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17), TPathAndPar] {
  def mapTo[P](cnv:Converter[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17),P]):PathAndParamsHResource[RootPath, P] = new PathAndParamsHResource[RootPath,P](h.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head.head, pathf17(h, cnv))
}

}