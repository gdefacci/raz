package org.obl.raz

sealed trait PathConcat[L, R] {

  type Out
  def concat(l: L, r: R): Out

}

sealed trait PathConcatAux[L, R, O] extends PathConcat[L, R] {

  type Out = O
  def concat(l: L, r: R): O

}

object PathConcat {

  def apply[L, R, O](f: (L, R) => O) = new PathConcatAux[L, R, O] {
    def concat(l: L, r: R): O = f(l, r)
  }

  import shapeless._
  import shapeless.ops.hlist._

  implicit def concatHLists[HL <: HList, HR <: HList, EL <: PathPosition, SR <: PathPosition](
    implicit prepend: Prepend[HL, HR],
    lEndsWith: EndsWith[HL, EL],
    rStartsWith: StartsWith[HR, SR],
    pathAppend:PathAppender[EL, SR]) =
    apply[HL, HR, prepend.Out]((l, r) => prepend.apply(l, r))

  implicit def concatPathEncoderToHList[TE, S <: PathPosition, E <: PathPosition, H <: HList, SH <: PathPosition](
    implicit hStartsWith: StartsWith[H, SH],
    pathAppender: PathAppender[E, SH]) = apply[PathEncoder[TE, S, E], H, PathEncoder[TE, S, E] :: H]((pc, h) => pc :: h)

    
  implicit def concatPathDecoderToHList[TD,S <: PathPosition, E <: PathPosition, H <: HList, SH <: PathPosition](
    implicit hStartsWith:StartsWith[H, SH],
    pathAppender:PathAppender[E, SH]
  ) = apply[PathDecoder[TD,S,E], H, PathDecoder[TD,S,E] :: H]( (pc, h) => pc :: h)

  implicit def concatPathCodecToHList[TD, TE, S <: PathPosition, E <: PathPosition, H <: HList, SH <: PathPosition](
    implicit hStartsWith: StartsWith[H, SH],
    pathAppender: PathAppender[E, SH]) = apply[PathCodec[TD, TE, S, E], H, PathCodec[TD, TE, S, E] :: H]((pc, h) => pc :: h)

  implicit def concatPathConverterToHList[TD, TE, TU, S <: PathPosition, E <: PathPosition, H <: HList, SH <: PathPosition](
    implicit hStartsWith: StartsWith[H, SH],
    pathAppender: PathAppender[E, SH]) = apply[PathConverter[TD, TE, TU, S, E], H, PathConverter[TD, TE, TU, S, E] :: H]((pc, h) => pc :: h)

}