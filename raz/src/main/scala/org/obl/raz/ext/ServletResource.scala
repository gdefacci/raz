package org.obl.raz
package ext

import scala.language.implicitConversions

class ServletResource[S <: PathPosition](
    val base: TPath[S, PathPosition.Segment],
    val servletPath: TPath[PathPosition.Segment, PathPosition.Segment],
    val segments: TPath[PathPosition.Segment, PathPosition.Segment] = Path.empty) {

  protected lazy val self = base append servletPath append segments

  def pathMatchDecoder: PathMatchDecoder = PathMatchDecoder(self, DecodeUtils.subtract(self, base append servletPath).getOrElse(self))

}

object ServletResource {
  
  implicit def toPathOps[S <: PathPosition](p: ServletResource[S]) = new PathOps(p)
  
  implicit def addTPath[S <: PathPosition] =
    SegmentAdd[ServletResource[S], ServletResource[S]]((resource, segment) =>
      new ServletResource[S](resource.base, resource.servletPath, resource.segments / segment))

  implicit def addConverterToTResource[S0 <: PathPosition, TD, TE, TU, S1 <: PathPosition, E1 <: PathPosition](
    implicit sgPathApp: PathAppender[PathPosition.Segment, S1]) =
    PathVarAdd[ServletResource[S0], PathConverter[TD, TE, TU, S1, E1], PathConverter[TD, TE, TU, S0, E1]] { (resource, pc) =>
      val pth = resource.base append resource.servletPath append resource.segments
      pc.prepend(pth).decoderAt(resource.base append resource.servletPath)
    }

}