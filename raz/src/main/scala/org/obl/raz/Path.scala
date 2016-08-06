package org.obl.raz

import scalaz.{ \/ => \/ }
import language.implicitConversions

sealed case class Path(scheme: Option[Scheme], authority: Option[Authority], segments: Seq[String], params: Seq[(String, Option[String])], fragment: Option[String]) {

  lazy val render = PathRenderer.render(this)

  def merge(p1: Path): Path =
    Path(scheme.orElse(p1.scheme), authority.orElse(p1.authority), segments ++ p1.segments, params ++ p1.params, fragment.orElse(p1.fragment))

}

object Path extends TPath[PathPosition.Segment, PathPosition.Segment](None, None, Nil, Nil, None) { 

  type SegmentsPath = TPath[PathPosition.Segment, PathPosition.Segment]
  
  def isEmpty(p: Path) = p.authority.isEmpty && p.segments.isEmpty && p.params.isEmpty && p.fragment.isEmpty

  def empty[P <: PathPosition] = new TPath[P, P](None, None, Nil, Nil, None)

  def segments(sgs:String*):TPath[PathPosition.Segment,PathPosition.Segment] = 
    TPath.apply[PathPosition.Segment,PathPosition.Segment](segments=sgs)
  
  def params(sgs:(String, Option[String])*):TPath[PathPosition.Param,PathPosition.Param] = 
    TPath.apply[PathPosition.Param,PathPosition.Param](params=sgs)
    
  def fragment(frag:String):TPath[PathPosition.Fragment,PathPosition.Fragment] = 
    TPath.apply[PathPosition.Fragment,PathPosition.Fragment](fragment=Some(frag))  
    
  def fromJavaUrl(u: java.net.URL): Throwable \/ Path = DecodeUtils.fromJavaUrl(u) 
    
  def fromJavaUri(uri: java.net.URI): Throwable \/ Path = DecodeUtils.fromJavaUri(uri)
  
  def toJavaUrl(p: Path): java.net.URL =
    new java.net.URI(p.render).toURL()

}

object TPath {
  
  private[raz] def apply[S1 <: PathPosition, E1 <: PathPosition](
    scheme: Option[Scheme] = None,
    authority: Option[Authority] = None,
    segments: Seq[String] = Nil,
    params: Seq[(String, Option[String])] = Nil,
    fragment: Option[String] = None): TPath[S1, E1] = {
    new TPath[S1, E1](scheme, authority, segments, params, fragment)
  }

  
  implicit def toPathOps[S <: PathPosition, E <: PathPosition](p:TPath[S,E]) = new PathOps(p)
}

sealed class TPath[S <: PathPosition, E <: PathPosition] private[raz] (
  scheme: Option[Scheme],
  authority: Option[Authority],
  segments: Seq[String],
  params: Seq[(String, Option[String])],
  fragment: Option[String]) extends Path(scheme, authority, segments, params, fragment) {
//    with PathBuilderMixin[S, E] {
  protected val self: TPath[S, E] = this

  def append[S1 <: PathPosition, E1 <: PathPosition](p1: TPath[S1, E1])(implicit appender:PathAppender[E, S1]): TPath[S, E1] =
    new TPath[S, E1](scheme.orElse(p1.scheme), authority.orElse(p1.authority), segments ++ p1.segments, params ++ p1.params, fragment.orElse(p1.fragment))

  def withAuthority(authority: Authority): TPath[PathPosition.Absolute, E] = new TPath[PathPosition.Absolute, E](scheme, Some(authority), segments, params, fragment)
  def withScheme(scheme: Scheme): TPath[PathPosition.Absolute, E] = new TPath[PathPosition.Absolute, E](Some(scheme), authority, segments, params, fragment)
}