package org.obl.raz

import shapeless._
import shapeless.ops.hlist.Prepend
import scala.language.implicitConversions
import shapeless.ops.hlist.Tupler

trait PathCodecBuilderMixin[H <: HList, S0 <: PathPosition, E <: PathPosition] {
  def hlist:H

  def pathCodec[TD,TE](implicit pcd:ToPathCodec[H, TD, TE, S0, E]) =
    pcd(hlist)
}

case class PathCodecBuilder[H <: HList, HR <: HList, S0 <: PathPosition, TD, TE, S <: PathPosition, E <: PathPosition](hlist:H, init:HR, tail:PathCodec[TD,TE,S,E]) 
  extends PathCodecBuilderMixin[H,S0,E] with PathEncoderBuilderMixin[H,S0,E] with PathDecoderBuilderMixin[H,S0,E] {

  def ++ [H1 <: HList, HR1 <: HList, S01 <: PathPosition, TD1,TE1,S1 <: PathPosition, E1 <: PathPosition](b:PathCodecBuilder[H1,HR1,S01,TD1,TE1,S1,E1]) 
    (implicit prepend : Prepend[H, H1], prepend1 : Prepend[H, HR1], pathAppender:PathAppender[E, S01]):PathCodecBuilder[prepend.Out, prepend1.Out, S0, TD1,TE1,S1,E1] =
    PathCodecBuilder[prepend.Out, prepend1.Out, S0, TD1,TE1, S1, E1](prepend(hlist, b.hlist), prepend1(hlist, b.init), b.tail)

  def / [TD1, TE1, S1 >: PathPosition.Segment <:PathPosition, E1 <: PathPosition](sg:PathCodec[TD1, TE1, S1, E1])
    (implicit pathAppender:PathAppender[E, PathPosition.Segment], prepend : Prepend[H, PathCodec[TD1, TE1, S1, E1] :: HNil]):PathCodecBuilder[prepend.Out,H,S0,TD1,TE1,S1,E1] =
      PathCodecBuilder(prepend.apply(hlist, sg :: HNil), hlist, sg)

  def && [TD1, TE1, E1 <: PathPosition](sg:PathCodec[TD1, TE1, PathPosition.Param, E1])
    (implicit pathAppender:PathAppender[E, PathPosition.Param], prepend : Prepend[H, PathCodec[TD1, TE1, PathPosition.Param, E1] :: HNil]):PathCodecBuilder[prepend.Out,H,S0,TD1,TE1,PathPosition.Param,E1] =
      PathCodecBuilder(prepend.apply(hlist, sg :: HNil), hlist, sg)

  def &# [TD1, TE1](sg:PathCodec[TD1, TE1, PathPosition.Fragment, PathPosition.Fragment])
    (implicit pathAppender:PathAppender[E, PathPosition.Fragment], prepend : Prepend[H, PathCodec[TD1, TE1, PathPosition.Fragment, PathPosition.Fragment] :: HNil]):PathCodecBuilder[prepend.Out,H,S0,TD1,TE1,PathPosition.Fragment,PathPosition.Fragment] =
      PathCodecBuilder(prepend.apply(hlist, sg :: HNil), hlist, sg)    
  
  def appendPath[S1 <:PathPosition, E1 <: PathPosition] (pth:TPath[S1,E1])(implicit pathAppender:PathAppender[E, S1], prepend:Prepend[HR, PathCodec[TD,TE,S,E1] :: HNil]) = {
    val pe = tail.append(pth)
    PathCodecBuilder[prepend.Out, HR, S0, TD, TE, S, E1](prepend.apply(init, pe :: HNil), init, pe)
  }     

  def / (sg:String)(implicit pathAppender:PathAppender[E, PathPosition.Segment], prepend:Prepend[HR, PathCodec[TD,TE,S,PathPosition.Segment] :: HNil]) = {
    appendPath(Path / sg)
  }    

  def && (sg:String)(implicit pathAppender:PathAppender[E, PathPosition.Param], prepend:Prepend[HR, PathCodec[TD,TE,S,PathPosition.Param] :: HNil]) = {
    appendPath(Path && sg)
  }    
  
  def && (name:String, value:String)(implicit pathAppender:PathAppender[E, PathPosition.Param], prepend:Prepend[HR, PathCodec[TD,TE,S,PathPosition.Param] :: HNil]) = {
    appendPath(Path && (name, value))
  }
  
  def &# (name:String)(implicit pathAppender:PathAppender[E, PathPosition.Fragment], prepend:Prepend[HR, PathCodec[TD,TE,S,PathPosition.Fragment] :: HNil]) = {
    appendPath(Path &# name)
  }

}