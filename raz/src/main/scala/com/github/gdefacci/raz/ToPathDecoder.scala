package com.github.gdefacci.raz

import shapeless._
import shapeless.ops.hlist.Tupler

trait ToPathDecoder[H, T, S <: PathPosition, E <: PathPosition] {
  def apply(h: H): PathDecoder[T, S, E]
}

object ToPathDecoder {

  implicit def htuple[H <: HList, HR <: HList, TUP, S <: PathPosition, E <: PathPosition](
      implicit hr: HPathDecoder[H, HR, S, E], 
      tupler: Tupler.Aux[HR, TUP], 
      gen: Generic.Aux[TUP, HR]) =
        
    new ToPathDecoder[H, TUP, S, E] {
      def apply(h: H) = PathDecoder { (pth: Path) =>
        hr.apply(h).apply(pth).map { mr =>
          MatchResult(tupler.apply(mr.value), mr.rest)
        }
      }
    }

  implicit def fromPathConverter[TD,TE,TU, S <: PathPosition, E <: PathPosition] =
    new ToPathDecoder[PathConverter[TD,TE,TU,S,E], TD, S, E] {
      def apply(h: PathConverter[TD,TE,TU,S,E]): PathDecoder[TD, S, E] = h.decoder
    }
  
  implicit def fromPathCodec[TD,TE,S <: PathPosition, E <: PathPosition] =
    new ToPathDecoder[PathCodec[TD,TE,S,E], TD, S, E] {
      def apply(h: PathCodec[TD,TE,S,E]): PathDecoder[TD, S, E] = h.decoder
    }
  
  implicit def pathDecoderIdentity[TD,S <: PathPosition, E <: PathPosition] =
    new ToPathDecoder[PathDecoder[TD,S,E], TD, S, E] {
      def apply(h: PathDecoder[TD,S,E]): PathDecoder[TD, S, E] = h
    }
    
}