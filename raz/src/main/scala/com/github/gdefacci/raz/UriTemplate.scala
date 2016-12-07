package com.github.gdefacci.raz

import shapeless._
import shapeless.ops.hlist.Tupler
import scala.language.implicitConversions 

object UriTemplate extends TUriTemplate[PathPosition.Segment, PathPosition.Segment](None, None, Nil, Nil, None) {// with RootPathBuilder[TUriTemplate] {
  
  def apply(scheme:Scheme, authority:Authority) = 
    new TUriTemplate[PathPosition.Absolute, PathPosition.Segment](scheme=Some(scheme), authority = Some(authority), segments = Nil, params = Nil, fragment = None)

  def apply(path:Path) = 
    new TUriTemplate[PathPosition, PathPosition](
        scheme = path.scheme, 
        authority = path.authority, 
        segments = path.segments.map(Segment(_)), 
        params = path.params.map(p => Param(ParamName(p._1), ParamValue(p._2))), 
        fragment = path.fragment.map(Fragment(_)))
    
  def segments(sgs:String*):TUriTemplate[PathPosition.Segment,PathPosition.Segment] = 
    new TUriTemplate[PathPosition.Segment,PathPosition.Segment](segments=sgs.map(Segment(_)) )
  
  def params(sgs:(String, Option[String])*):TUriTemplate[PathPosition.Param,PathPosition.Param] = 
    new TUriTemplate[PathPosition.Param,PathPosition.Param](params=sgs.map(p => Param(ParamName(p._1), ParamValue(p._2))))
    
  def fragment(frag:String):TUriTemplate[PathPosition.Fragment,PathPosition.Fragment] = 
    new TUriTemplate[PathPosition.Fragment,PathPosition.Fragment](fragment=Some(Fragment(frag)))  
      
  def segment(sg: PlaceHolder) =
    new TUriTemplate[PathPosition.Segment, PathPosition.Segment](None, None, Segment(sg) :: Nil, Nil, None)

  def param(name: String, value: PlaceHolder) =
    new TUriTemplate[PathPosition.Param, PathPosition.Param](None, None, Nil, Param(ParamName(name) -> ParamValue(value)) :: Nil, None)
 
  def param(par: PlaceHolder) =
    new TUriTemplate[PathPosition.Param, PathPosition.Param](None, None, Nil, Param(par) :: Nil, None)
    
  def fragment(frag: PlaceHolder) =
    new TUriTemplate[PathPosition.Fragment, PathPosition.Fragment](None, None, Nil, Nil, Some(UriTemplate.Fragment(frag)))
  
  sealed class PlaceHolderOr[T](val value: Either[PlaceHolder, T])

  final case class PlaceHolder(name: String)

  final case class Segment(v: Either[PlaceHolder, String]) extends PlaceHolderOr[String](v)
  final case class ParamName(v: Either[PlaceHolder, String]) extends PlaceHolderOr[String](v)
  final case class ParamValue(v: Either[PlaceHolder, Option[String]]) extends PlaceHolderOr[Option[String]](v)
  final case class Param(v: Either[PlaceHolder, (ParamName, ParamValue)]) extends PlaceHolderOr[(ParamName, ParamValue)](v)
  final case class Fragment(v: Either[PlaceHolder, String]) extends PlaceHolderOr[String](v)

  
  sealed class PartFactory[T, R](val apply: Either[PlaceHolder, T] => R) {
    def apply(t: PlaceHolder): R = apply(Left(t))
    def apply(t: T): R = apply(Right(t))
  }
  
  object Segment extends PartFactory[String, Segment](new Segment(_))
  object ParamName extends PartFactory[String, ParamName](new ParamName(_))
  object ParamValue extends PartFactory[Option[String], ParamValue](new ParamValue(_))
  object Param extends PartFactory[(ParamName, ParamValue), Param](new Param(_)) {
    
    def apply(p:(String, Option[String])):Param = Param(ParamName(p._1) -> ParamValue(p._2))
  }

  object Fragment extends PartFactory[String, Fragment](new Fragment(_))
  final case class ParamWithName(name:String)

}

import UriTemplate.PlaceHolder

sealed case class UriTemplate(scheme:Option[Scheme], authority: Option[Authority], segments: Seq[UriTemplate.Segment], params: Seq[UriTemplate.Param], fragment: Option[UriTemplate.Fragment]) {
  
  lazy val render = PathRenderer.render(this)
  
}

object TUriTemplate {
  
  implicit def toPathOps[S <: PathPosition, E <: PathPosition](p:TUriTemplate[S,E]) = new PathOps(p)
  
}

class TUriTemplate[S <: PathPosition, E <: PathPosition] private[raz] (
    scheme:Option[Scheme] = None, 
    authority: Option[Authority] = None, 
    segments: Seq[UriTemplate.Segment] = Nil, 
    params: Seq[UriTemplate.Param] = Nil, 
    fragment: Option[UriTemplate.Fragment] = None) extends UriTemplate(scheme, authority, segments, params, fragment) { 
  
  def /(sg: PlaceHolder)(implicit appender: PathAppender[E, PathPosition.Segment]) =
    new TUriTemplate[S, PathPosition.Segment](scheme, authority, segments :+ UriTemplate.Segment(sg), params, fragment)

  def &&(name: String, value: PlaceHolder)(implicit appender: PathAppender[E, PathPosition.Param]) =
    new TUriTemplate[S, PathPosition.Param](scheme, authority, segments, params :+ UriTemplate.Param(UriTemplate.ParamName(name) -> UriTemplate.ParamValue(value)), fragment)
    
  def &#(frag: PlaceHolder)(implicit appender: PathAppender[E, PathPosition.Fragment]) =
    new TUriTemplate[S, PathPosition.Fragment](scheme, authority, segments, params, Some(UriTemplate.Fragment(frag)))

  def append[S1 <: PathPosition, E1 <: PathPosition](p1:TUriTemplate[S1,E1])(implicit appender:PathAppender[E, S1]):TUriTemplate[S,E1] = 
    new TUriTemplate[S,E1](scheme.orElse(p1.scheme), authority.orElse(p1.authority), segments ++ p1.segments, params ++ p1.params, fragment.orElse(p1.fragment))
  
}