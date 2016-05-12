package org.obl.raz
package ext

import shapeless.{ HNil, :: }

trait Resource[S >: PathPosition.Segment <: PathPosition] {

  protected val self: TPath[S, PathPosition.Segment]

  protected lazy val prefix = self

  def pathMatchDecoder: PathMatchDecoder

  protected def shift[TD1, TE1, UT, S1 >: PathPosition.Segment <: PathPosition, E1 <: PathPosition](sg: PathConverter[TD1, TE1, UT, S1, E1])(
    implicit pathAppender: PathAppender[PathPosition.Segment, S1]): PathConverter[TD1, TE1, UT, S, E1]

  def /[TD1, TE1, UT, S1 >: PathPosition.Segment <: PathPosition, E1 <: PathPosition](sg: PathConverter[TD1, TE1, UT, S1, E1])(
    implicit pathAppender: PathAppender[PathPosition.Segment, S1]) = {
    PathConverter.toPathConverterBuilder(shift(sg))
  }

}

class WebSocketResource[S >: PathPosition.Segment <: PathPosition](
    base: TPath[S, PathPosition.Segment],
    appPath: Path.SegmentsPath,
    segments: Path.SegmentsPath) extends Resource[S] {

  protected lazy val self = base append appPath append segments

  def pathMatchDecoder: PathMatchDecoder = PathMatchDecoder(self, DecodeUtils.subtract(self, base).getOrElse(self))

  def /(sg: String) =
    new WebSocketResource(base, appPath, segments / sg)

  protected def shift[TD1, TE1, UT, S1 >: PathPosition.Segment <: PathPosition, E1 <: PathPosition](sg: PathConverter[TD1, TE1, UT, S1, E1])(
      implicit pathAppender: PathAppender[PathPosition.Segment, S1]): PathConverter[TD1, TE1, UT, S, E1] =
    sg.prepend(self).decoderAt(base).uriTemplateEncoderAt(base.append(appPath))

}

class ServletResource[S >: PathPosition.Segment <: PathPosition](
    base: TPath[S, PathPosition.Segment],
    servletPath: Path.SegmentsPath,
    segments: Path.SegmentsPath = Path.empty) extends Resource[S] {

  protected lazy val self = base append servletPath append segments

  def pathMatchDecoder: PathMatchDecoder = PathMatchDecoder(self, DecodeUtils.subtract(self, base append servletPath).getOrElse(self))

  def /(sg: String) =
    new ServletResource(base, servletPath, segments / sg)

  protected def shift[TD1, TE1, UT, S1 >: PathPosition.Segment <: PathPosition, E1 <: PathPosition](sg: PathConverter[TD1, TE1, UT, S1, E1])(
      implicit pathAppender: PathAppender[PathPosition.Segment, S1]): PathConverter[TD1, TE1, UT, S, E1] =
    sg.prepend(self).decoderAt(base append servletPath)

}

