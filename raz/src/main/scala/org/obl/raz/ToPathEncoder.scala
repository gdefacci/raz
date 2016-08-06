package org.obl.raz

import shapeless._
import shapeless.ops.hlist.Tupler

trait ToPathEncoder[H, T, S <: PathPosition, E <: PathPosition] {
  def apply(h: H): PathEncoder[T, S, E]
}

object ToPathEncoder {

  implicit def htuple[H <: HList, HR <: HList, TUP, S <: PathPosition, E <: PathPosition](
      implicit hr: HPathEncoder[H, HR, S, E], 
      tupler: Tupler.Aux[HR, TUP], 
      gen: Generic.Aux[TUP, HR]) =
    new ToPathEncoder[H, TUP, S, E] {
      def apply(h: H): PathEncoder[TUP, S, E] = {
        val hrEnc = hr.apply(h)
        PathEncoder({ (tup: TUP) =>
          val h1 = gen.to(tup)
          hrEnc.apply(h1)
        })
      }
    }

//  implicit def htuple1[T, S <: PathPosition, E <: PathPosition] =
//    new ToPathEncoder[PathEncoder[T,S,E] :: HNil, T, S, E] {
//      def apply(h: PathEncoder[T,S,E] :: HNil): PathEncoder[T, S, E] = {
//        h.head
//      }
//    }
  
  implicit def fromPathConverter[TD,TE,TU, S <: PathPosition, E <: PathPosition] =
    new ToPathEncoder[PathConverter[TD,TE,TU,S,E], TE, S, E] {
      def apply(h: PathConverter[TD,TE,TU,S,E]): PathEncoder[TE, S, E] = h.encoder
    }

  implicit def pathEncoderIdentity[TE,S <: PathPosition, E <: PathPosition] =
    new ToPathEncoder[PathEncoder[TE,S,E], TE, S, E] {
      def apply(h: PathEncoder[TE,S,E]): PathEncoder[TE, S, E] = h
    }
}