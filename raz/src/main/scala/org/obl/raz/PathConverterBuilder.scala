package org.obl.raz

import shapeless._
import shapeless.ops.hlist.Prepend
import scala.language.implicitConversions
import shapeless.ops.hlist.Tupler
import scala.language.higherKinds

trait PathConverterBuilderMixin[H <: HList, S0 <: PathPosition, E <: PathPosition] {
  def hlist:H

  def pathConverter[TD,TE,TU](implicit pcd:ToPathConverter[H, TD, TE, TU, S0, E]) =
    pcd(hlist)
  
  def pathCodec[TD,TE](implicit pcd:ToPathCodec[H, TD, TE, S0, E]):PathCodec[TD, TE, S0, E]  =
    pcd(hlist)
}

case class PathConverterBuilder[H <: HList, HR <: HList, S0 <: PathPosition, TD, TE, UT, S <: PathPosition, E <: PathPosition](hlist:H, init:HR, tail:PathConverter[TD,TE,UT,S,E]) 
  extends PathConverterBuilderMixin[H,S0,E] with PathEncoderBuilderMixin[H,S0,E] with PathDecoderBuilderMixin[H,S0,E] {
  
  private def createPathConverterBuilder[H1 <: HList, HR1 <: HList, S01 <: PathPosition, TD1, TE1, UT1, S1 <: PathPosition, E1 <: PathPosition](
      hlist:H1, init:HR1, 
      tail:PathConverter[TD1,TE1,UT1,S1,E1]) = PathConverterBuilder[H1,HR1,S01,TD1,TE1,UT1,S1,E1](hlist, init, tail)
      
  def ++ [H1 <: HList, HR1 <: HList, S01 <: PathPosition, TD1,TE1,UT1, S1 <: PathPosition, E1 <: PathPosition](b:PathConverterBuilder[H1,HR1,S01,TD1,TE1,UT1,S1,E1]) 
    (implicit prepend : Prepend[H, H1], prepend1 : Prepend[H, HR1], pathAppender:PathAppender[E, S01]):PathConverterBuilder[prepend.Out, prepend1.Out, S0, TD1,TE1,UT1,S1,E1] =
    createPathConverterBuilder[prepend.Out, prepend1.Out, S0, TD1,TE1,UT1, S1, E1](prepend(hlist, b.hlist), prepend1(hlist, b.init), b.tail)
  
  def / [TD1, TE1,  UT1,S1 >: PathPosition.Segment <:PathPosition, E1 <: PathPosition](sg:PathConverter[TD1, TE1,  UT1,S1, E1])
    (implicit pathAppender:PathAppender[E, PathPosition.Segment], prepend : Prepend[H, PathConverter[TD1, TE1,  UT1,S1, E1] :: HNil]):PathConverterBuilder[prepend.Out,H,S0,TD1,TE1,UT1,S1,E1] =
      createPathConverterBuilder(prepend.apply(hlist, sg :: HNil), hlist, sg)

  def && [TD1, TE1,  UT1,E1 <: PathPosition](sg:PathConverter[TD1, TE1,  UT1,PathPosition.Param, E1])
    (implicit pathAppender:PathAppender[E, PathPosition.Param], prepend : Prepend[H, PathConverter[TD1, TE1,  UT1,PathPosition.Param, E1] :: HNil]):PathConverterBuilder[prepend.Out,H,S0,TD1,TE1,UT1,PathPosition.Param,E1] =
      createPathConverterBuilder(prepend.apply(hlist, sg :: HNil), hlist, sg)

  def &# [TD1, TE1,  UT1](sg:PathConverter[TD1, TE1,  UT1,PathPosition.Fragment, PathPosition.Fragment])
    (implicit pathAppender:PathAppender[E, PathPosition.Fragment], prepend : Prepend[H, PathConverter[TD1, TE1,  UT1,PathPosition.Fragment, PathPosition.Fragment] :: HNil]):PathConverterBuilder[prepend.Out,H,S0,TD1,TE1,UT1,PathPosition.Fragment,PathPosition.Fragment] =
      createPathConverterBuilder(prepend.apply(hlist, sg :: HNil), hlist, sg)    
    
  def appendPath[S1 <:PathPosition, E1 <: PathPosition] (pth:TPath[S1,E1])(
      implicit pathAppender:PathAppender[E, S1], 
      prepend:Prepend[HR, PathConverter[TD,TE,UT,S,E1] :: HNil]):PathConverterBuilder[prepend.Out, HR, S0, TD, TE, UT, S, E1] = {
    val pe = tail.append(pth)
    createPathConverterBuilder[prepend.Out, HR, S0, TD, TE, UT, S, E1](prepend.apply(init, pe :: HNil), init, pe)
  }     
      
  def / (sg:String)(implicit pathAppender:PathAppender[E, PathPosition.Segment], prepend:Prepend[HR, PathConverter[TD,TE,UT,S,PathPosition.Segment] :: HNil]) = 
    appendPath(Path / sg)

  def && (sg:String)(implicit pathAppender:PathAppender[E, PathPosition.Param], prepend:Prepend[HR, PathConverter[TD,TE,UT,S,PathPosition.Param] :: HNil]) = 
    appendPath(Path && sg)
  
  def && (name:String, value:String)(implicit pathAppender:PathAppender[E, PathPosition.Param], prepend:Prepend[HR, PathConverter[TD,TE,UT,S,PathPosition.Param] :: HNil]) = 
    appendPath(Path && (name, value))
  
  def &# (name:String)(implicit pathAppender:PathAppender[E, PathPosition.Fragment], prepend:Prepend[HR, PathConverter[TD,TE,UT,S,PathPosition.Fragment] :: HNil]) = 
    appendPath(Path &# name)
  
}