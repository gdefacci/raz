package org.obl.raz

import scalaz.{ \/ => \/ }

sealed case class Path(scheme: Option[Scheme], authority: Option[Authority], segments: Seq[String], params: Seq[(String, Option[String])], fragment: Option[String]) {

  lazy val render = PathRenderer.render(this)

  def merge(p1: Path): Path =
    Path(scheme.orElse(p1.scheme), authority.orElse(p1.authority), segments ++ p1.segments, params ++ p1.params, fragment.orElse(p1.fragment))

}

object Path extends Path(None, None, Nil, Nil, None) with RootPathBuilder[TPath] with TPathPathBuilder with RootPathEncoderBuilder
    with RootPathDecoderBuilder
    with RootPathCodecBuilder
    with RootPathConverterBuilder {

  protected val self = this

  type SegmentsPath = TPath[PathPosition.Segment, PathPosition.Segment]

  def isEmpty(p: Path) = p.authority.isEmpty && p.segments.isEmpty && p.params.isEmpty && p.fragment.isEmpty

  def empty[P <: PathPosition] = new TPath[P, P](None, None, Nil, Nil, None)

  def fromJavaUrl(u: java.net.URL): Throwable \/ Path = DecodeUtils.fromJavaUrl(u) 
    
  def fromJavaUri(uri: java.net.URI): Throwable \/ Path = DecodeUtils.fromJavaUri(uri)
  
  def toJavaUrl(p: Path): java.net.URL =
    new java.net.URI(p.render).toURL()

}

trait TPathPathBuilder {
  protected def self: Path
  protected def create[S1 <: PathPosition, E1 <: PathPosition](
    scheme: Option[Scheme] = None,
    authority: Option[Authority] = None,
    segments: Seq[String] = Nil,
    params: Seq[(String, Option[String])] = Nil,
    fragment: Option[String] = None): TPath[S1, E1] = {
    new TPath[S1, E1](scheme.orElse(self.scheme), authority.orElse(self.authority), self.segments ++ segments, self.params ++ params, fragment.orElse(self.fragment))
  }

}

trait PathBuilderMixin[S <: PathPosition, E <: PathPosition] extends PathBuilder[S, E, TPath]
  with TPathPathBuilder
  with TPathEncoderBuilder[S, E]
  with TPathDecoderBuilder[S, E]
  with TPathCodecBuilder[S, E]
  with TPathConverterBuilder[S, E]

sealed class TPath[S <: PathPosition, E <: PathPosition] private[raz] (
  scheme: Option[Scheme],
  authority: Option[Authority],
  segments: Seq[String],
  params: Seq[(String, Option[String])],
  fragment: Option[String]) extends Path(scheme, authority, segments, params, fragment)
    with PathBuilderMixin[S, E] {
  protected val self: TPath[S, E] = this

  def append[S1 <: PathPosition, E1 <: PathPosition](p1: TPath[S1, E1]): TPath[S, E1] =
    new TPath[S, E1](scheme.orElse(p1.scheme), authority.orElse(p1.authority), segments ++ p1.segments, params ++ p1.params, fragment.orElse(p1.fragment))

  def withAuthority(authority: Authority): TPath[PathPosition.Absolute, E] = new TPath[PathPosition.Absolute, E](scheme, Some(authority), segments, params, fragment)
  def withScheme(scheme: Scheme): TPath[PathPosition.Absolute, E] = new TPath[PathPosition.Absolute, E](Some(scheme), authority, segments, params, fragment)
}