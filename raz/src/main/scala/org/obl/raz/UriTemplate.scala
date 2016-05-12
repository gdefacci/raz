package org.obl.raz

import shapeless._
import shapeless.ops.hlist.Tupler

object UriTemplate extends UriTemplate(None, None, Nil, Nil, None) with TUriTemplatePathBuilder with RootPathBuilder[TUriTemplate] {
  
  def apply(scheme:Scheme, authority:Authority) = 
    create[PathPosition.Absolute, PathPosition.Segment](scheme=Some(scheme), authority = Some(authority))

  def apply(path:Path) = 
    create[PathPosition, PathPosition](scheme = path.scheme, authority = path.authority, segments = path.segments, params = path.params, fragment = path.fragment)
    
  def /(sg: PlaceHolder) =
    new TUriTemplate[PathPosition.Segment, PathPosition.Segment](scheme, authority, segments :+ UriTemplate.Segment(sg), params, fragment)

  def &&(name: String, value: PlaceHolder) =
    new TUriTemplate[PathPosition.Param, PathPosition.Param](scheme, authority, segments, params :+ UriTemplate.Param(UriTemplate.ParamName(name) -> UriTemplate.ParamValue(value)), fragment)
 
  def &&(par: PlaceHolder) =
    new TUriTemplate[PathPosition.Param, PathPosition.Param](scheme, authority, segments, params :+ UriTemplate.Param(par), fragment)
    
  def &#(frag: PlaceHolder) =
    new TUriTemplate[PathPosition.Fragment, PathPosition.Fragment](scheme, authority, segments, params, Some(UriTemplate.Fragment(frag)))

  sealed class PlaceHolderOr[T](val value: Either[PlaceHolder, T])

  final case class PlaceHolder(name: String)

  final case class Segment(v: Either[PlaceHolder, String]) extends PlaceHolderOr[String](v)
  final case class ParamName(v: Either[PlaceHolder, String]) extends PlaceHolderOr[String](v)
  final case class ParamValue(v: Either[PlaceHolder, Option[String]]) extends PlaceHolderOr[Option[String]](v)
  final case class Param(v: Either[PlaceHolder, (ParamName, ParamValue)]) extends PlaceHolderOr[(ParamName, ParamValue)](v)
  final case class Fragment(v: Either[PlaceHolder, String]) extends PlaceHolderOr[String](v)

  sealed trait Kind
  
  sealed trait PartFactory[T, R] {
    def apply: Either[PlaceHolder, T] => R
    def apply(t: PlaceHolder): R = apply(Left(t))
    def apply(t: T): R = apply(Right(t))
  }

  object Segment extends PartFactory[String, Segment] with Kind {
    val apply = new Segment(_)
  }

  object ParamName extends PartFactory[String, ParamName] {
    val apply = new ParamName(_)
  }

  object ParamValue extends PartFactory[Option[String], ParamValue] {
    val apply = new ParamValue(_)
  }

  object Param extends PartFactory[(ParamName, ParamValue), Param] with Kind {
    val apply = new Param(_)
    
    def apply(p:(String, Option[String])):Param = Param(ParamName(p._1) -> ParamValue(p._2))
  }

  object Fragment extends PartFactory[String, Fragment] with Kind {
    val apply = new Fragment(_)
  }
  
  final case class ParamWithName(name:String) extends Kind

}

import UriTemplate.PlaceHolder

sealed case class UriTemplate(scheme:Option[Scheme], authority: Option[Authority], segments: Seq[UriTemplate.Segment], params: Seq[UriTemplate.Param], fragment: Option[UriTemplate.Fragment]) {
  
  lazy val render = PathRenderer.render(this)
  
}

trait TUriTemplatePathBuilder { self:UriTemplate =>
  
  protected def create[S1 <: PathPosition, E1 <: PathPosition](
		  scheme:Option[Scheme] = None, 
      authority:Option[Authority] = None, 
      segments:Seq[String] = Nil, 
      params:Seq[(String,Option[String])] = Nil, 
      fragment:Option[String] = None):TUriTemplate[S1, E1] = {
    new TUriTemplate[S1,E1](
        scheme.orElse(this.scheme),
        authority.orElse(this.authority), 
        this.segments ++ segments.map(sg => UriTemplate.Segment(sg)), 
        this.params ++ params.map(p => UriTemplate.Param(UriTemplate.ParamName(p._1), UriTemplate.ParamValue(p._2))), 
        fragment.map( f => UriTemplate.Fragment(f)).orElse(this.fragment))
  }
  
}

class TUriTemplate[S <: PathPosition, E <: PathPosition] private[raz] (
    scheme:Option[Scheme], 
    authority: Option[Authority], 
    segments: Seq[UriTemplate.Segment], 
    params: Seq[UriTemplate.Param], 
    fragment: Option[UriTemplate.Fragment]) extends UriTemplate(scheme, authority, segments, params, fragment) with PathBuilder[S,E, TUriTemplate] with TUriTemplatePathBuilder {
  
  def /(sg: PlaceHolder)(implicit appender: PathAppender[E, PathPosition.Segment]) =
    new TUriTemplate[S, PathPosition.Segment](scheme, authority, segments :+ UriTemplate.Segment(sg), params, fragment)

  def &&(name: String, value: PlaceHolder)(implicit appender: PathAppender[E, PathPosition.Param]) =
    new TUriTemplate[S, PathPosition.Param](scheme, authority, segments, params :+ UriTemplate.Param(UriTemplate.ParamName(name) -> UriTemplate.ParamValue(value)), fragment)
    
  def &#(frag: PlaceHolder)(implicit appender: PathAppender[E, PathPosition.Fragment]) =
    new TUriTemplate[S, PathPosition.Fragment](scheme, authority, segments, params, Some(UriTemplate.Fragment(frag)))

  def append[S1 <: PathPosition, E1 <: PathPosition](p1:TUriTemplate[S1,E1]):TUriTemplate[S,E1] = 
    new TUriTemplate[S,E1](scheme.orElse(p1.scheme), authority.orElse(p1.authority), segments ++ p1.segments, params ++ p1.params, fragment.orElse(p1.fragment))
  
}