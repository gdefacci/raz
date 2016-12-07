package com.github.gdefacci.raz

import shapeless._
import shapeless.ops.hlist.Tupler

trait ToPathConverter[H, TD, TE, TU, S <: PathPosition, E <: PathPosition] {
  def apply(h: H): PathConverter[TD, TE, TU, S, E]
}

object ToPathConverter {
  
  implicit def fromDecoderEncoderUriTemplate[H, TD, TE, UT, S <: PathPosition, E <: PathPosition]
    (implicit
        topd:ToPathDecoder[H, TD, S,E],
        tope:ToPathEncoder[H, TE, S,E],
        toue:ToUriTemplateEncoder[H, UT, S,E]) =
    new ToPathConverter[H, TD, TE, UT, S, E] {
      def apply(h: H) = PathConverter[TD, TE, UT, S, E](topd(h), tope(h), toue(h))
    }

  implicit def pathConverterIdentity[TD, TE, TU, S <: PathPosition, E <: PathPosition] =
    new ToPathConverter[PathConverter[TD, TE, TU, S, E], TD, TE, TU, S, E] {
      def apply(h: PathConverter[TD, TE, TU, S, E]): PathConverter[TD, TE, TU, S, E] = h
    }

}