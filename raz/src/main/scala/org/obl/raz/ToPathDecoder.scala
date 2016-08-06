package org.obl.raz

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

  implicit def htuple1[T, S <: PathPosition, E <: PathPosition] =
    new ToPathDecoder[PathDecoder[T,S,E] :: HNil, T, S, E] {
      def apply(h: PathDecoder[T,S,E] :: HNil): PathDecoder[T, S, E] = {
        h.head
      }
    }
  
  implicit def fromPathConverter[TD,TE,TU, S <: PathPosition, E <: PathPosition] =
    new ToPathDecoder[PathConverter[TD,TE,TU,S,E], TD, S, E] {
      def apply(h: PathConverter[TD,TE,TU,S,E]): PathDecoder[TD, S, E] = h.decoder
    }
    
}