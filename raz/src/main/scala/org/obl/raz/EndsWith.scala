package org.obl.raz

final case class EndsWith[T, E <: PathPosition] private () 

object EndsWith {
  
  implicit def pathEncoder[T,S <: PathPosition,E <: PathPosition] = EndsWith[PathEncoder[T,S,E], E]
  implicit def pathDecoder[T,S <: PathPosition,E <: PathPosition] = EndsWith[PathDecoder[T,S,E], E]
  implicit def pathCodec[TD,TE,S <: PathPosition,E <: PathPosition] = EndsWith[PathCodec[TD,TE,S,E], E]
	implicit def pathConverter[TD,TE,TU,S <: PathPosition,E <: PathPosition] = EndsWith[PathConverter[TD,TE,TU,S,E], E]
  
  import shapeless._
  
  implicit def hcons1[T, E <: PathPosition](implicit endsWith:EndsWith[T,E]) =
    EndsWith[T :: HNil, E]
  
  implicit def hcons[H <: HList, T, E <: PathPosition](implicit endsWith:EndsWith[H,E]) =
    EndsWith[T :: H, E]

  
  
}