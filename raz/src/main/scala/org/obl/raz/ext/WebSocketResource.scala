package org.obl.raz
package ext

import scala.language.implicitConversions

class WebSocketResource[S <: PathPosition](
    val base: TPath[S, PathPosition.Segment],
    val appPath: TPath[PathPosition.Segment, PathPosition.Segment],
    val segments: TPath[PathPosition.Segment, PathPosition.Segment]) {

  protected lazy val self = base append appPath append segments

  def pathMatchDecoder: PathMatchDecoder = PathMatchDecoder(self, DecodeUtils.subtract(self, base).getOrElse(self))

}

object WebSocketResource {
  
  implicit def toPathOps[S <: PathPosition](p: WebSocketResource[S]) = new PathOps(p)
  
  implicit def addTPath[S <: PathPosition] =
    SegmentAdd[WebSocketResource[S], WebSocketResource[S]]((resource, segment) =>
      new WebSocketResource[S](resource.base, resource.appPath, resource.segments / segment))

  implicit def addConverterToTResource[S0 <: PathPosition, TD, TE, TU, S1 <: PathPosition, E1 <: PathPosition](
    implicit sgPathApp: PathAppender[PathPosition.Segment, S1]) =
    PathVarAdd[WebSocketResource[S0], PathConverter[TD, TE, TU, S1, E1], PathConverter[TD, TE, TU, S0, E1]] { (resource, pc) =>
      val pth = resource.base append resource.appPath append resource.segments
      pc.prepend(pth).decoderAt(resource.base).uriTemplateEncoderAt(resource.base.append(resource.appPath))
    }

}

object WebSocketResourceMain extends App {

  val base = Path / "base"
  val appPath = Path / "app"

  val rsc = new WebSocketResource(base, appPath, Path.empty) / "aa" / PathConverter.Segment.string

  class Res1 extends WebSocketResource(base, appPath, Path.empty) {
    val pippo = this / "" / PathConverter.Segment.string / PathConverter.Segment.string
  }
  
  println(rsc.pathEncoder.encode("par"))
  println(rsc.pathConverter.decode(Path / "app" / "aa" / "v"))
  println(rsc.pathConverter.uriTemplateEncoder.encodeUriTemplate("par"))

}

