package com.github.gdefacci.raz

final case class StartsWith[T, S <: PathPosition] private () 

object StartsWith {
  
  implicit def pathEncoder[T,S <: PathPosition,E <: PathPosition] = StartsWith[PathEncoder[T,S,E], S]
  implicit def pathDecoder[T,S <: PathPosition,E <: PathPosition] = StartsWith[PathDecoder[T,S,E], S]
  implicit def pathCodec[TD,TE,S <: PathPosition,E <: PathPosition] = StartsWith[PathCodec[TD,TE,S,E], S]
	implicit def pathConverter[TD,TE,TU,S <: PathPosition,E <: PathPosition] = StartsWith[PathConverter[TD,TE,TU,S,E], S]
  
  import shapeless._
  
  implicit def hcons1[H <: HList, T, S <: PathPosition](implicit startsWith:StartsWith[T,S]) =
    StartsWith[T :: H, S]

  
}