package org.obl.raz

import shapeless._
import shapeless.ops.hlist.Prepend
import scala.language.implicitConversions
import shapeless.ops.hlist.Tupler

trait PathEncoderBuilderMixin[H <: HList, S0 <: PathPosition, E <: PathPosition] {
  def hlist:H
  
  def pathEncoder[T](implicit tpe:ToPathEncoder[H,T,S0,E]) = tpe(hlist)
  
  def uriTemplateEncoder[T](implicit tue:ToUriTemplateEncoder[H,T,S0,E]):UriTemplateEncoder[T, S0, E] = tue(hlist)
  
}

case class PathEncoderBuilder[H <: HList, HR <: HList, S0 <: PathPosition, T, S <: PathPosition, E <: PathPosition](hlist:H, init:HR, tail:PathEncoder[T,S,E]) 
  extends PathEncoderBuilderMixin[H,S0,E] {
  
  def ++ [H1 <: HList, HR1 <: HList, S01 <: PathPosition, T1, S1 <: PathPosition, E1 <: PathPosition](b:PathEncoderBuilder[H1,HR1,S01,T1,S1,E1]) 
    (implicit prepend : Prepend[H, H1], prepend1 : Prepend[H, HR1], pathAppender:PathAppender[E, S01]):PathEncoderBuilder[prepend.Out, prepend1.Out, S0, T1,S1,E1] =
    PathEncoderBuilder[prepend.Out, prepend1.Out, S0, T1, S1, E1](prepend(hlist, b.hlist), prepend1(hlist, b.init), b.tail)
  
  def / [T1, S1 >: PathPosition.Segment <:PathPosition, E1 <: PathPosition](sg:PathEncoder[T1, S1, E1])
    (implicit pathAppender:PathAppender[E, PathPosition.Segment], prepend : Prepend[H, PathEncoder[T1, S1, E1] :: HNil]):PathEncoderBuilder[prepend.Out,H,S0,T1,S1,E1] =
      PathEncoderBuilder(prepend.apply(hlist, sg :: HNil), hlist, sg)

  def && [T1, E1 <: PathPosition](sg:PathEncoder[T1, PathPosition.Param, E1])
    (implicit pathAppender:PathAppender[E, PathPosition.Param], prepend : Prepend[H, PathEncoder[T1, PathPosition.Param, E1] :: HNil]):PathEncoderBuilder[prepend.Out,H,S0,T1,PathPosition.Param,E1] =
      PathEncoderBuilder(prepend.apply(hlist, sg :: HNil), hlist, sg)

  def &# [T1](sg:PathEncoder[T1, PathPosition.Fragment, PathPosition.Fragment])
    (implicit pathAppender:PathAppender[E, PathPosition.Fragment], prepend : Prepend[H, PathEncoder[T1, PathPosition.Fragment, PathPosition.Fragment ] :: HNil]):PathEncoderBuilder[prepend.Out,H,S0,T1,PathPosition.Fragment,PathPosition.Fragment] =
      PathEncoderBuilder(prepend.apply(hlist, sg :: HNil), hlist, sg)    

  def appendPath[S1 <: PathPosition, E1 <: PathPosition](sg:TPath[S1,E1])(implicit pathAppender:PathAppender[E, S1], prepend:Prepend[HR, PathEncoder[T,S,E1] :: HNil]) = {
    val pe = tail.append(sg)
    PathEncoderBuilder[prepend.Out, HR, S0, T, S, E1](prepend.apply(init, pe :: HNil), init, pe)
  }
      
  def / (sg:String)(implicit pathAppender:PathAppender[E, PathPosition.Segment], prepend:Prepend[HR, PathEncoder[T,S,PathPosition.Segment] :: HNil]) = 
    appendPath(Path / sg)
  
  def && (sg:String)(implicit pathAppender:PathAppender[E, PathPosition.Param], prepend:Prepend[HR, PathEncoder[T,S,PathPosition.Param] :: HNil]) = 
    appendPath(Path && sg)
  
  def && (name:String, value:String)(implicit pathAppender:PathAppender[E, PathPosition.Param], prepend:Prepend[HR, PathEncoder[T,S,PathPosition.Param] :: HNil]) = 
    appendPath(Path && (name, value))
  
  def &# (name:String)(implicit pathAppender:PathAppender[E, PathPosition.Fragment], prepend:Prepend[HR, PathEncoder[T,S,PathPosition.Fragment] :: HNil]) = 
    appendPath(Path &# name)

}