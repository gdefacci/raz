package org.obl.raz

import scalaz.{ -\/, \/, \/- }
import scala.language.implicitConversions
import scala.annotation.unchecked.uncheckedVariance
import scala.language.existentials

sealed trait PathConverterKind
sealed trait SimplePathConverterKind extends PathConverterKind

object PathConverterKind {
  case class Generic(shape:HPath) extends PathConverterKind
  case class Segment(suffix:Path) extends SimplePathConverterKind 
  case class Param(suffix:Path) extends SimplePathConverterKind 
  case class ParamWithName(name:String, suffix:Path) extends SimplePathConverterKind 
}

class PathConverter[D, E, UT,+P <: PathPosition, S <: PathPosition] private (d: Path => Throwable \/ PathMatchResult[D, Path], e: E => Path, ute: UT => UriTemplate, val kind:PathConverterKind) extends PathDecoder[D] with PathEncoder[E] with UriTemplateEncoder[UT] {
  def decode(path: Path) = d(path)
  def encode(t: E): Path = e(t)
  def toUriTemplate(t: UT) = ute(t)

  type Decoder[T] = PathConverter[T, E, UT, P @uncheckedVariance, S]
  type Encoder[T] = PathConverter[D, T, UT, P @uncheckedVariance, S]
  type UTEncoder[T] = PathConverter[D, E, T, P @uncheckedVariance, S]

  protected def createDecoder[T1](f: Path => Throwable \/ PathMatchResult[T1, Path]) = PathConverter[T1, E, UT, P, S](PathDecoder(f), e, ute, kind)
  protected def createEncoder[T1](f: T1 => Path) = PathConverter[D, T1, UT, P, S](PathDecoder(d), f, ute, kind)
  protected def createUriTemplateEncoder[T1](f: T1 => UriTemplate) = PathConverter[D, E, T1, P, S](PathDecoder(d), e, UriTemplateEncoder.apply[T1](f).toUriTemplate(_), kind)

  private[raz] def addPart(sg: PathSg):PathConverter[D, E, UT, P, SegmentPosition] = addPath(RelativePath(sg))
  private[raz] def addPart(sg: QParamSg):PathConverter[D, E, UT, P, ParamPosition] = addPath(RelativePath(sg :: Nil))
  private[raz] def addFragmentPart(f: String):PathConverter[D, E, UT, P, FragmentPosition] = addPath(RelativePath(fragment = f))

  private[raz] def addPath[A1 <: PathPosition](p: BasePath[_, A1]):PathConverter[D, E, UT, P, A1] = {
    val nkind:PathConverterKind = kind match {
      case PathConverterKind.Generic(HPathCons(h, pc)) => {
        val nshape = HPathCons(h, pc.addPath(p))  
        PathConverterKind.Generic(nshape)
      }
      case PathConverterKind.Generic(HPathNil(h)) => {
        val nshape = HPathNil(BasePath.cast[PathPosition, PathPosition](PathUtils.merge(h,p)))  
        PathConverterKind.Generic(nshape)
      }
      case PathConverterKind.Segment(suffix) => PathConverterKind.Segment(PathUtils.merge(suffix,p))  
      case PathConverterKind.Param(suffix) => PathConverterKind.Param(PathUtils.merge(suffix,p))  
      case PathConverterKind.ParamWithName(name, suffix) => PathConverterKind.ParamWithName(name, PathUtils.merge(suffix,p))  
    }
    PathConverter[D, E, UT, P, A1](PathDecoder.withSuffix(this, p), PathEncoder.withSuffix(this, p), UriTemplateEncoder.withSuffix(this, p), nkind)
  }

  def caseMap[T1](tupled: D => T1, caseUnapply: T1 => Option[E]): PathConverter[T1, T1, UT, P, S] = {
    PathConverter(
        PathDecoder(d.andThen(_.flatMap( p => \/.fromTryCatchNonFatal(p.mapValue(tupled) ))))
        , e.compose((t1: T1) => caseUnapply(t1).get), ute, kind)
  }

}

object PathConverter {

  def apply[D, E, UT, P <: PathPosition, S <: PathPosition](d: PathDecoder[D], e: PathEncoder[E], ute: UriTemplateEncoder[UT], kind:PathConverterKind): PathConverter[D, E, UT, P, S] = {
    new PathConverter[D, E, UT, P, S](d.decode _, e.encode _, ute.toUriTemplate _, kind)
  }
  
//  def apply[D, E, UT, P <: PathPosition, S <: PathPosition](d: PathDecoder[D], e: PathEncoder[E], ute: UriTemplateEncoder[UT], kind:SimplePathConverterKind): PathConverter[D, E, UT, P, S] = {
//    new PathConverter[D, E, UT, P, S](d.decode _, e.encode _, ute.toUriTemplate _, kind)
//  }

  def apply[D, E, UT, P <: PathPosition, S <: PathPosition](d: Path => Throwable \/ PathMatchResult[D, Path], e: E => Path, ute: UT => UriTemplate): PathConverter[D, E, UT, P, S] = {
    PathConverter[D, E, UT, P, S](d, e, ute)
  }
  
//  def fromPath(p:Path) = apply(PathDecoder.fromPath(p), PathEncoder.fromPath(p), UriTemplateEncoder { p:Path => UriTemplate(p) })
//  
//  implicit def fromBasePath[P <: PathPosition, S <: P](p:BasePath[P,S]) = 
//    apply[Path,Path,Path,P,S](PathDecoder.fromPath(p), PathEncoder.fromPath(p), UriTemplateEncoder { p:Path => UriTemplate(p) })
  
  def apply[H <: HPath, P <: PathPosition, S <: P, D, E, UT](h: H)(implicit pathMatcher: PathMatcher[H, D], hf: EncHPathF[H, E, BasePath[P, S]], uthf: UTHPathF[H, UT]): PathConverter[D, E, UT, P, S] = {
    val d = pathMatcher.decoder(h)
    val e = hf.apply(h)
    val ute = uthf.apply(h)
    PathConverter(d, PathEncoder(e), UriTemplateEncoder(ute), PathConverterKind.Generic(h))
  }

//  private def factory[P <: PathPosition, S <: PathPosition] = new PathConverterFactory[P, S]

  def opt[D, E, UT, P <: PathPosition, S <: PathPosition](pc: PathConverter[D, E, UT, P, S]): PathConverter[Option[D], Option[E], Option[UT], P, S] = {
    PathConverter(PathDecoder.opt(pc), PathEncoder.opt(pc), UriTemplateEncoder.opt(pc), pc.kind)
  }

  def seq[D, E, UT, P <: PathPosition, S <: PathPosition](pc: PathConverter[D, E, UT, P, S]): PathConverter[Seq[D], Seq[E], UT, P, S] = {
    PathConverter[Seq[D], Seq[E], UT, P, S](PathDecoder.seq(pc), PathEncoder.seq(pc), UriTemplateEncoder.expand(pc), pc.kind)
  }

  private class PathConverterFactory[P <: PathPosition, S <: PathPosition] {
    def create[D, E, UT](kind:PathConverterKind)(d: PathDecoder[D], e: PathEncoder[E], ute: UriTemplateEncoder[UT]): PathConverter[D, E, UT, P, S] = {
      PathConverter[D, E, UT, P, S](d, e, ute, kind)
    }
  }
  
  object Segment {
    def factory[D,E,UT] = new PathConverterFactory[SegmentPosition, SegmentPosition]().create[D,E,UT](PathConverterKind.Segment(RelativePath))(_,_,_)

    val string = factory(PathDecoder.stringSegment, PathEncoder.stringSegment, UriTemplateEncoder.Simple.segment)
    val int = factory(PathDecoder.intSegment, PathEncoder.intSegment, UriTemplateEncoder.Simple.segment)
    val long = factory(PathDecoder.longSegment, PathEncoder.longSegment, UriTemplateEncoder.Simple.segment)
    val boolean = factory(PathDecoder.booleanSegment, PathEncoder.booleanSegment, UriTemplateEncoder.Simple.segment)
    
    def enumSegment[E <: Enumeration](e:E) = factory(PathDecoder.enumSegment(e), PathEncoder.enumSegment[E], UriTemplateEncoder.Simple.segment)
  }

  object Param {
    def factory[D,E,UT] = new PathConverterFactory[ParamPosition, ParamPosition].create[D,E,UT](PathConverterKind.Param(RelativePath))(_,_,_)
    def withNamefactory[D,E,UT](name:String) = new PathConverterFactory[ParamPosition, ParamPosition].create[D,E,UT](PathConverterKind.ParamWithName(name, RelativePath))(_,_,_)

    val string = factory(PathDecoder.stringParam, PathEncoder.stringParam, UriTemplateEncoder.Simple.param)
    val int = factory(PathDecoder.intParam, PathEncoder.intParam, UriTemplateEncoder.Simple.param)
    val long = factory(PathDecoder.longParam, PathEncoder.longParam, UriTemplateEncoder.Simple.param)
    val boolean = factory(PathDecoder.booleanParam, PathEncoder.booleanParam, UriTemplateEncoder.Simple.param)

    def string(parName: String): PathConverter[String, String, String, ParamPosition, ParamPosition] =
      withNamefactory(parName)(
        PathDecoder.stringParamValue(parName),
        PathEncoder.stringParamValue(parName),
        UriTemplateEncoder.Simple.paramNamed(parName))

    def int(parName: String): PathConverter[Int, Int, String, ParamPosition, ParamPosition] =
      withNamefactory(parName)(
        PathDecoder.intParamValue(parName),
        PathEncoder.intParamValue(parName),
        UriTemplateEncoder.Simple.paramNamed(parName))

    def long(parName: String): PathConverter[Long, Long, String, ParamPosition, ParamPosition] =
      withNamefactory(parName)(
        PathDecoder.longParamValue(parName),
        PathEncoder.longParamValue(parName),
        UriTemplateEncoder.Simple.paramNamed(parName))

    def boolean(parName: String): PathConverter[Boolean, Boolean, String, ParamPosition, ParamPosition] =
      withNamefactory(parName)(
        PathDecoder.booleanParamValue(parName),
        PathEncoder.booleanParamValue(parName),
        UriTemplateEncoder.Simple.paramNamed(parName))

        
    def enum[E <: Enumeration](e:E, parName:String) = 
      withNamefactory(parName)(PathDecoder.enumParamValue(e)(parName), PathEncoder.enumParamValue[E](parName), UriTemplateEncoder.Simple.paramNamed(parName))    
  }

}