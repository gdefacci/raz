package com.github.gdefacci.raz

import shapeless._
import shapeless.ops.hlist.Prepend

trait PathVarAdd[A, B] {

  type Out

  def add(a: A, b: B): Out

}

trait PathVarAddAux[A, B, O] extends PathVarAdd[A, B] {

  type Out = O

}

object PathVarAdd {

  def apply[A, B, O](f: (A, B) => O) = new PathVarAddAux[A, B, O] {
    def add(a: A, b: B): O = f(a, b)
  }

  implicit def addEncoderToTPath[S0 <: PathPosition, E0 <: PathPosition, T, S1 <: PathPosition, E1 <: PathPosition](
    implicit sgPathApp: PathAppender[E0, S1]) =
    apply[TPath[S0, E0], PathEncoder[T, S1, E1], PathEncoder[T, S0, E1]] { (a, segment) => LeftPathEncoder(a, segment) }

  implicit def addEncoders[T0, S0 <: PathPosition, E0 <: PathPosition, T1, S1 <: PathPosition, E1 <: PathPosition](implicit sgPathApp: PathAppender[E0, S1]) =
    apply[PathEncoder[T0, S0, E0], PathEncoder[T1, S1, E1], PathEncoder[T0, S0, E0] :: PathEncoder[T1, S1, E1] :: HNil] { (p1, p2) => p1 :: p2 :: HNil }

  implicit def addEncoderToHNil[T1, S1 <: PathPosition, E1 <: PathPosition] =
    apply[HNil, PathEncoder[T1, S1, E1], PathEncoder[T1, S1, E1] :: HNil] { (p1, p2) => p2 :: p1 }

  implicit def addEncoderToHCons[T1, S1 <: PathPosition, E1 <: PathPosition, H <: HList, EH <: PathPosition](
    implicit prepend: Prepend[H, PathEncoder[T1, S1, E1] :: HNil],
    hEndsWith: EndsWith[H, EH],
    hpathApp: PathAppender[EH, S1]) =
    apply[H, PathEncoder[T1, S1, E1], prepend.Out] { (p1, p2) => prepend.apply(p1, p2 :: HNil) }

  implicit def addDecoderToTPath[S0 <: PathPosition, E0 <: PathPosition, T, S1 <: PathPosition, E1 <: PathPosition](
    implicit sgPathApp: PathAppender[E0, S1], sfxPathApp: PathAppender[E1, E1]) =
    apply[TPath[S0, E0], PathDecoder[T, S1, E1], PathDecoder[T, S0, E1]] { (a, segment) => LeftPathDecoder(a, segment) }

  implicit def addDecoders[T0, S0 <: PathPosition, E0 <: PathPosition, T1, S1 <: PathPosition, E1 <: PathPosition](implicit sgPathApp: PathAppender[E0, S1]) =
    apply[PathDecoder[T0, S0, E0], PathDecoder[T1, S1, E1], PathDecoder[T0, S0, E0] :: PathDecoder[T1, S1, E1] :: HNil] { (p1, p2) => p1 :: p2 :: HNil }

  implicit def addDecoderToHNil[T1, S1 <: PathPosition, E1 <: PathPosition] =
    apply[HNil, PathDecoder[T1, S1, E1], PathDecoder[T1, S1, E1] :: HNil] { (p1, p2) => p2 :: p1 }

  implicit def addDecoderToHCons[T1, S1 <: PathPosition, E1 <: PathPosition, H <: HList, EH <: PathPosition](
    implicit prepend: Prepend[H, PathDecoder[T1, S1, E1] :: HNil],
    hEndsWith: EndsWith[H, EH],
    hpathApp: PathAppender[EH, S1]) =
    apply[H, PathDecoder[T1, S1, E1], prepend.Out] { (p1, p2) => prepend.apply(p1, p2 :: HNil) }
  
  implicit def addCodecToTPath[S0 <: PathPosition, E0 <: PathPosition, TD, TE,  S1 <: PathPosition, E1 <: PathPosition](
    implicit sgPathApp: PathAppender[E0, S1], sfxPathApp: PathAppender[E1, E1]) =
    apply[TPath[S0, E0], PathCodec[TD, TE,  S1, E1], PathCodec[TD, TE,  S0, E1]] { (a, segment) => segment.prepend(a) }

  implicit def addCodecs[TD0, TE0,  S0 <: PathPosition, E0 <: PathPosition, TD1, TE1,  S1 <: PathPosition, E1 <: PathPosition](
    implicit sgPathApp: PathAppender[E0, S1]) =
    apply[PathCodec[TD0, TE0,  S0, E0], PathCodec[TD1, TE1,  S1, E1], PathCodec[TD0, TE0,  S0, E0] :: PathCodec[TD1, TE1,  S1, E1] :: HNil] { (p1, p2) => p1 :: p2 :: HNil }

  implicit def addCodecToHNil[TD1, TE1,  S1 <: PathPosition, E1 <: PathPosition] =
    apply[HNil, PathCodec[TD1, TE1,  S1, E1], PathCodec[TD1, TE1,  S1, E1] :: HNil] { (p1, p2) => p2 :: p1 }

  implicit def addCodecToHCons[TD1, TE1,  E0 <: PathPosition, S1 <: PathPosition, E1 <: PathPosition, H <: HList, EH <: PathPosition](
    implicit prepend: Prepend[H, PathCodec[TD1, TE1,  S1, E1] :: HNil],
    hEndsWith: EndsWith[H, EH],
    hpathApp: PathAppender[EH, S1]) =
    apply[H, PathCodec[TD1, TE1,  S1, E1], prepend.Out] { (p1, p2) => prepend.apply(p1, p2 :: HNil) }

  implicit def addConverterToTPath[S0 <: PathPosition, E0 <: PathPosition, TD, TE, TU, S1 <: PathPosition, E1 <: PathPosition](
    implicit sgPathApp: PathAppender[E0, S1], sfxPathApp: PathAppender[E1, E1]) =
    apply[TPath[S0, E0], PathConverter[TD, TE, TU, S1, E1], PathConverter[TD, TE, TU, S0, E1]] { (a, segment) => segment.prepend(a) }

  implicit def addConverters[TD0, TE0, TU0, S0 <: PathPosition, E0 <: PathPosition, TD1, TE1, TU1, S1 <: PathPosition, E1 <: PathPosition](
    implicit sgPathApp: PathAppender[E0, S1]) =
    apply[PathConverter[TD0, TE0, TU0, S0, E0], PathConverter[TD1, TE1, TU1, S1, E1], PathConverter[TD0, TE0, TU0, S0, E0] :: PathConverter[TD1, TE1, TU1, S1, E1] :: HNil] { (p1, p2) => p1 :: p2 :: HNil }

  implicit def addConverterToHNil[TD1, TE1, TU1, S1 <: PathPosition, E1 <: PathPosition] =
    apply[HNil, PathConverter[TD1, TE1, TU1, S1, E1], PathConverter[TD1, TE1, TU1, S1, E1] :: HNil] { (p1, p2) => p2 :: p1 }

  implicit def addConverterToHCons[TD1, TE1, TU1, E0 <: PathPosition, S1 <: PathPosition, E1 <: PathPosition, H <: HList, EH <: PathPosition](
    implicit prepend: Prepend[H, PathConverter[TD1, TE1, TU1, S1, E1] :: HNil],
    hEndsWith: EndsWith[H, EH],
    hpathApp: PathAppender[EH, S1]) =
    apply[H, PathConverter[TD1, TE1, TU1, S1, E1], prepend.Out] { (p1, p2) => prepend.apply(p1, p2 :: HNil) }

}
