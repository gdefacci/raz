package org.obl.raz

import scala.language.higherKinds
import shapeless._
import shapeless.ops.hlist.IsHCons
import org.obl.raz.PathConverter.Fragment

trait PathBuilder[S <: PathPosition, E <: PathPosition, P[_ <: PathPosition,_ <: PathPosition]] {
  
  protected def create[S1 <: PathPosition, E1 <: PathPosition](
		  scheme:Option[Scheme] = None, 
      authority:Option[Authority] = None, 
      segments:Seq[String] = Nil, 
      params:Seq[(String,Option[String])] = Nil, 
      fragment:Option[String] = None):P[S1, E1]
  
   def / (sg:String)(implicit appender:PathAppender[E, PathPosition.Segment]) = 
    create[S,PathPosition.Segment](segments = Seq(sg))
  
  def && (name:String, value:String)(implicit appender:PathAppender[E, PathPosition.Param]) = 
    create[S,PathPosition.Param](params  = Seq(name -> Some(value)))
  
  def && (name:String)(implicit appender:PathAppender[E, PathPosition.Param]) = 
    create[S,PathPosition.Param](params  = Seq(name -> None))
  
  def &# (frag:String)(implicit appender:PathAppender[E, PathPosition.Fragment]) = 
    create[S,PathPosition.Fragment](fragment = Some(frag))
    
  def at(scheme:Scheme, authority:Authority) = 
    create[PathPosition.Absolute, E](scheme = Some(scheme), authority = Some(authority))
}

trait RootPathBuilder[P[_ <: PathPosition,_ <: PathPosition]] {
  
  protected def create[S1 <: PathPosition, E1 <: PathPosition](
      scheme:Option[Scheme] = None, 
      authority:Option[Authority] = None, 
      segments:Seq[String] = Nil, 
      params:Seq[(String,Option[String])] = Nil, 
      fragment:Option[String] = None):P[S1, E1]
  
   def / (sg:String)(implicit appender:PathAppender[PathPosition.Segment, PathPosition.Segment]) = 
    create[PathPosition.Segment,PathPosition.Segment](segments = Seq(sg))
  
  def && (name:String, value:String)(implicit appender:PathAppender[PathPosition.Param, PathPosition.Param]) = 
    create[PathPosition.Param,PathPosition.Param](params  = Seq(name -> Some(value)))
  
  def && (name:String)(implicit appender:PathAppender[PathPosition.Param, PathPosition.Param]) = 
    create[PathPosition.Param,PathPosition.Param](params  = Seq(name -> None))
  
  def &# (frag:String)(implicit appender:PathAppender[PathPosition.Param, PathPosition.Fragment]) = 
    create[PathPosition.Fragment,PathPosition.Fragment](fragment = Some(frag))
    
}

trait TPathEncoderBuilder[S <: PathPosition, E <: PathPosition] { 
  
  protected def self:TPath[S,E]
  
  protected def createEncoderBuilder[T1, S1 <:PathPosition, E1 <: PathPosition](sg:PathEncoder[T1, S1, E1])
    (implicit pathAppender: PathAppender[E, S1]) =
      PathEncoder.toPathEncoderBuilder( sg.prepend(self) )

  def / [T1, S1 >: PathPosition.Segment <:PathPosition, E1 <: PathPosition](sg:PathEncoder[T1, S1, E1])
    (implicit pathAppender: PathAppender[E, S1]):PathEncoderBuilder[PathEncoder[T1, S, E1] :: HNil, HNil, S, T1, S, E1] = 
      createEncoderBuilder(sg)
  
  def && [T1, E1 <: PathPosition](sg:PathEncoder[T1, PathPosition.Param, E1])
    (implicit pathAppender:PathAppender[E, PathPosition.Param]):PathEncoderBuilder[PathEncoder[T1, S, E1] :: HNil, HNil, S, T1, S, E1] = 
      createEncoderBuilder(sg)

  def &# [T1](sg:PathEncoder[T1, PathPosition.Fragment, PathPosition.Fragment])
    (implicit pathAppender:PathAppender[E, PathPosition.Fragment]):PathEncoderBuilder[PathEncoder[T1, S, PathPosition.Fragment] :: HNil, HNil, S, T1, S, PathPosition.Fragment] = 
      createEncoderBuilder(sg)
}

trait TPathDecoderBuilder[S <: PathPosition, E <: PathPosition] { 

  protected def self:TPath[S,E]
  def pathMatchDecoder : PathMatchDecoder = PathMatchDecoder(self)
  
  protected def createDecoderBuilder[T1, S1 <:PathPosition, E1 <: PathPosition](sg:PathDecoder[T1, S1, E1])
    (implicit pathAppender: PathAppender[E, S1]) =
      PathDecoder.fromPathDecoderBuilder( sg.prepend(self) )

  def / [T1, S1 >: PathPosition.Segment <:PathPosition, E1 <: PathPosition](sg:PathDecoder[T1, S1, E1])
    (implicit pathAppender: PathAppender[E, S1]):PathDecoderBuilder[PathDecoder[T1, S, E1] :: HNil, HNil, S, T1, S, E1] = 
      createDecoderBuilder(sg)
  
  def && [T1, E1 <: PathPosition](sg:PathDecoder[T1, PathPosition.Param, E1])
    (implicit pathAppender:PathAppender[E, PathPosition.Param]):PathDecoderBuilder[PathDecoder[T1, S, E1] :: HNil, HNil, S, T1, S, E1] = 
      createDecoderBuilder(sg)

  def &# [T1](sg:PathDecoder[T1, PathPosition.Fragment, PathPosition.Fragment])
    (implicit pathAppender:PathAppender[E, PathPosition.Fragment]):PathDecoderBuilder[PathDecoder[T1, S, PathPosition.Fragment] :: HNil, HNil, S, T1, S, PathPosition.Fragment] = 
      createDecoderBuilder(sg)
  
}

trait TPathCodecBuilder[S <: PathPosition, E <: PathPosition] { 

  protected def self:TPath[S,E]

  protected def createCodecBuilder[TD1, TE1, S1 <:PathPosition, E1 <: PathPosition](sg:PathCodec[TD1, TE1, S1, E1])
    (implicit pathAppender: PathAppender[E, S1]):PathCodecBuilder[PathCodec[TD1, TE1, S, E1] :: HNil, HNil, S, TD1, TE1, S, E1] = {
    PathCodec.toPathCodecBuilder( sg.prepend(self) )  
  }
  
  def / [TD1, TE1, S1 >: PathPosition.Segment <:PathPosition, E1 <: PathPosition](sg:PathCodec[TD1, TE1, S1, E1])
    (implicit pathAppender: PathAppender[E, S1]):PathCodecBuilder[PathCodec[TD1, TE1, S, E1] :: HNil, HNil, S, TD1, TE1, S, E1] = 
      createCodecBuilder(sg)
  
  def && [TD1, TE1, E1 <: PathPosition](sg:PathCodec[TD1, TE1, PathPosition.Param, E1])
    (implicit pathAppender:PathAppender[E, PathPosition.Param]):PathCodecBuilder[PathCodec[TD1, TE1, S, E1] :: HNil, HNil, S, TD1, TE1, S, E1] = 
      createCodecBuilder(sg)

  def &# [TD1, TE1](sg:PathCodec[TD1, TE1, PathPosition.Fragment, PathPosition.Fragment])
    (implicit pathAppender:PathAppender[E, PathPosition.Fragment]):PathCodecBuilder[PathCodec[TD1, TE1, S, PathPosition.Fragment] :: HNil, HNil, S, TD1, TE1, S, PathPosition.Fragment] = 
      createCodecBuilder(sg)
}

trait TPathConverterBuilder[S <: PathPosition, E <: PathPosition] { 

  protected def self:TPath[S,E]
  
  protected def createConverterBuilder[TD1, TE1, UT, S1 <:PathPosition, E1 <: PathPosition](sg:PathConverter[TD1, TE1, UT, S1, E1])
    (implicit pathAppender: PathAppender[E, S1]):PathConverterBuilder[PathConverter[TD1, TE1, UT, S, E1] :: HNil, HNil, S, TD1, TE1, UT, S, E1] = {
    PathConverter.toPathConverterBuilder( sg.prepend(self) )  
  }

  def / [TD1, TE1, UT, S1 >: PathPosition.Segment <:PathPosition, E1 <: PathPosition](sg:PathConverter[TD1, TE1, UT, S1, E1])
    (implicit pathAppender: PathAppender[E, S1]):PathConverterBuilder[PathConverter[TD1, TE1, UT, S, E1] :: HNil, HNil, S, TD1, TE1, UT, S, E1] = 
      createConverterBuilder(sg)
  
  def && [TD1, TE1, UT, E1 <: PathPosition](sg:PathConverter[TD1, TE1,UT, PathPosition.Param, E1])
    (implicit pathAppender:PathAppender[E, PathPosition.Param]):PathConverterBuilder[PathConverter[TD1, TE1, UT, S, E1] :: HNil, HNil, S, TD1, TE1, UT, S, E1] = 
      createConverterBuilder(sg)

  def &# [TD1, TE1, UT](sg:PathConverter[TD1, TE1, UT, PathPosition.Fragment, PathPosition.Fragment])
    (implicit pathAppender:PathAppender[E, PathPosition.Fragment]):PathConverterBuilder[PathConverter[TD1, TE1, UT, S, PathPosition.Fragment] :: HNil, HNil, S, TD1, TE1, UT, S, PathPosition.Fragment] = 
      createConverterBuilder(sg)

}

trait RootPathEncoderBuilder { 

  import PathPosition._
  import PathEncoder.toPathEncoderBuilder

  def / [T1, S1 >: Segment <:PathPosition, E1 <: PathPosition](sg:PathEncoder[T1, S1, E1]):PathEncoderBuilder[PathEncoder[T1, S1, E1] :: HNil, HNil, S1, T1, S1, E1] = 
      toPathEncoderBuilder(sg)
  
  def && [T1, E1 <: PathPosition](sg:PathEncoder[T1, Param, E1]):PathEncoderBuilder[PathEncoder[T1, Param, E1] :: HNil, HNil, Param, T1, Param, E1] = 
      toPathEncoderBuilder(sg)

  def &# [T1](sg:PathEncoder[T1, Fragment, Fragment]):PathEncoderBuilder[PathEncoder[T1, Fragment, Fragment] :: HNil, HNil, Fragment, T1, PathPosition.Fragment, PathPosition.Fragment] = 
     toPathEncoderBuilder(sg)
}

trait RootPathDecoderBuilder { 

  import PathPosition._
  import PathDecoder.fromPathDecoderBuilder
  
  def / [T1, S1 >: Segment <:PathPosition, E1 <: PathPosition](sg:PathDecoder[T1, S1, E1]):PathDecoderBuilder[PathDecoder[T1, S1, E1] :: HNil, HNil, S1, T1, S1, E1] = 
      fromPathDecoderBuilder(sg)
  
  def && [T1, E1 <: PathPosition](sg:PathDecoder[T1, Param, E1]):PathDecoderBuilder[PathDecoder[T1, Param, E1] :: HNil, HNil, Param, T1, Param, E1] = 
      fromPathDecoderBuilder(sg)

  def &# [T1](sg:PathDecoder[T1, Fragment, Fragment]):PathDecoderBuilder[PathDecoder[T1, Fragment, Fragment] :: HNil, HNil, Fragment, T1, Fragment, Fragment] = 
      fromPathDecoderBuilder(sg)
  
}

trait RootPathCodecBuilder { 

  import PathPosition._
  import PathCodec.toPathCodecBuilder
  
  def / [TD1, TE1, S1 >: Segment <:PathPosition, E1 <: PathPosition](sg:PathCodec[TD1, TE1, S1, E1]):PathCodecBuilder[PathCodec[TD1, TE1, S1, E1] :: HNil, HNil, S1, TD1, TE1, S1, E1] = 
      toPathCodecBuilder(sg)
  
  def && [TD1, TE1, E1 <: PathPosition](sg:PathCodec[TD1, TE1, Param, E1]):PathCodecBuilder[PathCodec[TD1, TE1, Param, E1] :: HNil, HNil, Param, TD1, TE1, Param, E1] = 
      toPathCodecBuilder(sg)

  def &# [TD1, TE1](sg:PathCodec[TD1, TE1, Fragment, Fragment]):PathCodecBuilder[PathCodec[TD1, TE1, Fragment, Fragment] :: HNil, HNil, Fragment, TD1, TE1, Fragment, Fragment] = 
      toPathCodecBuilder(sg)
}

trait RootPathConverterBuilder { 

  import PathPosition._
  import PathConverter.toPathConverterBuilder

  def / [TD1, TE1, UT, E1 <: PathPosition](sg:PathConverter[TD1, TE1, UT, Segment, E1]):PathConverterBuilder[PathConverter[TD1, TE1, UT, Segment, E1] :: HNil, HNil, Segment, TD1, TE1, UT, Segment, E1] = 
      toPathConverterBuilder(sg)
  
  def && [TD1, TE1, UT, E1 <: PathPosition](sg:PathConverter[TD1, TE1,UT, Param, E1]):PathConverterBuilder[PathConverter[TD1, TE1, UT, Param, E1] :: HNil, HNil, Param, TD1, TE1, UT, Param, E1] = 
      toPathConverterBuilder(sg)

  def &# [TD1, TE1, UT](sg:PathConverter[TD1, TE1, UT, Fragment, Fragment]):PathConverterBuilder[PathConverter[TD1, TE1, UT, Fragment, Fragment] :: HNil, HNil, Fragment, TD1, TE1, UT, Fragment, Fragment] = 
      toPathConverterBuilder(sg)
  
}