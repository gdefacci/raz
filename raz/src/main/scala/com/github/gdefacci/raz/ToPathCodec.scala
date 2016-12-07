package com.github.gdefacci.raz

import shapeless._

trait ToPathCodec[H, TD, TE, S <: PathPosition, E <: PathPosition] {
  def apply(h: H): PathCodec[TD, TE, S, E]
}

object ToPathCodec {

  implicit def fromDecoderAndEncoder[H <: HList, TD, TE, S <: PathPosition, E <: PathPosition](
    implicit topd: ToPathDecoder[H, TD, S, E], 
    tope: ToPathEncoder[H, TE, S, E]) =
    new ToPathCodec[H, TD, TE, S, E] {
      def apply(h: H) = PathCodec(topd(h), tope(h))
    }

  implicit def pathCodecIdentity[TD, TE, S <: PathPosition, E <: PathPosition] =
    new ToPathCodec[PathCodec[TD, TE, S, E], TD, TE, S, E] {
      def apply(h: PathCodec[TD, TE, S, E]): PathCodec[TD, TE, S, E] = h
    }

  implicit def fromPathConverter[TD, TE, TU, S <: PathPosition, E <: PathPosition] =
    new ToPathCodec[PathConverter[TD, TE, TU, S, E], TD, TE, S, E] {
      def apply(h: PathConverter[TD, TE, TU, S, E]): PathCodec[TD, TE, S, E] = h.toCodec
    }

}
