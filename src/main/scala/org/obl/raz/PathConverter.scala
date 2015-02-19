package org.obl.raz

import scalaz.{ -\/, \/, \/- }

class PathConverter[D, E, UT, A <: CanAddAspect, P <: CanHavePrefixAspect](d: Path => Throwable \/ PathMatchResult[D, Path], e: E => Path, ute: UT => UriTemplate) extends PathDecoder[D] with PathEncoder[E] with UriTemplateEncoder[UT] {
  def decode(path: Path) = d(path)
  def encode(t: E): Path = e(t)
  def toUriTemplate(t: UT) = ute(t)

  type Decoder[T] = PathConverter[T, E, UT, A, P]
  type Encoder[T] = PathConverter[D, T, UT, A, P]
  type UTEncoder[T] = PathConverter[D, E, T, A, P]

  protected def createDecoder[T1](f: Path => Throwable \/ PathMatchResult[T1, Path]) = new PathConverter[T1, E, UT, A, P](f, e, ute)
  protected def createEncoder[T1](f: T1 => Path) = new PathConverter[D, T1, UT, A, P](d, f, ute)
  protected def createUriTemplateEncoder[T1](f: T1 => UriTemplate) = new PathConverter[D, E, T1, A, P](d, e, UriTemplateEncoder.apply[T1](f).toUriTemplate(_))

  private[raz] def addPart(sg: PathSg) = addPath[CanAddPath](Path(sg))
  private[raz] def addPart(sg: QParamSg) = addPath[CanAddParam](Path(sg :: Nil))
  private[raz] def addFragmentPart(f: String) = addPath[CanAddAspect](Path.empty.copy(fragment = Some(f)))

  private[raz] def addPath[A1 <: CanAddAspect](p: Path) =
    PathConverter[D, E, UT, A1, P](PathDecoder.withSuffix(this, p), PathEncoder.withSuffix(this, p), UriTemplateEncoder.withSuffix(this, p))

  def caseMap[T1](tupled: D => T1, caseUnapply: T1 => Option[E]): PathConverter[T1, T1, UT, A, P] = {
    new PathConverter(d.andThen(_.flatMap( p => \/.fromTryCatch(p.mapValue(tupled) ))), e.compose((t1: T1) => caseUnapply(t1).get), ute)
  }

}

object PathConverter {

  def apply[D, E, UT, A <: CanAddAspect, P <: CanHavePrefixAspect](d: PathDecoder[D], e: PathEncoder[E], ute: UriTemplateEncoder[UT]): PathConverter[D, E, UT, A, P] = {
    new PathConverter[D, E, UT, A, P](d.decode _, e.encode _, ute.toUriTemplate _)
  }

  def apply[D, E, UT, A <: CanAddAspect, P <: CanHavePrefixAspect](d: Path => Throwable \/ PathMatchResult[D, Path], e: E => Path, ute: UT => UriTemplate): PathConverter[D, E, UT, A, P] = {
    PathConverter[D, E, UT, A, P](d, e, ute)
  }
  
  def fromPath(p:Path) = apply(PathDecoder.fromPath(p), PathEncoder.fromPath(p), UriTemplateEncoder { p:Path => UriTemplate(p) })

  implicit def apply[H <: HPath, R <: RelativePathAspect, A <: CanAddAspect, P <: CanHavePrefixAspect, D, E, UT](h: H)(implicit pathMatcher: PathMatcher[H, D], hf: EncHPathF[H, E, BasePath[R, A, P]], uthf: UTHPathF[H, UT]): PathConverter[D, E, UT, A, P] = {
    val d = pathMatcher.decoder(h)
    val e = hf.apply(h)
    val ute = uthf.apply(h)
    PathConverter(d, PathEncoder(e), UriTemplateEncoder(ute))
  }

  private def factory[A <: CanAddAspect, P <: CanHavePrefixAspect] = new PathConverterFactory[A, P]

  def opt[D, E, UT, A <: CanAddAspect, P <: CanHavePrefixAspect](pc: PathConverter[D, E, UT, A, P]): PathConverter[Option[D], Option[E], Option[UT], A, P] = {
    PathConverter(PathDecoder.opt(pc), PathEncoder.opt(pc), UriTemplateEncoder.opt(pc))
  }

  def seq[D, E, UT, A <: CanAddAspect, P <: CanHavePrefixAspect](pc: PathConverter[D, E, UT, A, P]): PathConverter[Seq[D], Seq[E], UT, A, P] = {
    PathConverter[Seq[D], Seq[E], UT, A, P](PathDecoder.seq(pc), PathEncoder.seq(pc), UriTemplateEncoder.expand(pc))
  }

  object Segment {
    val factory = PathConverter.factory[CanAddPath, CanHavePathAsPrefix]

    val string = factory.create(PathDecoder.stringSegment, PathEncoder.stringSegment, UriTemplateEncoder.Simple.segment)
    val int = factory.create(PathDecoder.intSegment, PathEncoder.intSegment, UriTemplateEncoder.Simple.segment)
    val long = factory.create(PathDecoder.longSegment, PathEncoder.longSegment, UriTemplateEncoder.Simple.segment)
    val boolean = factory.create(PathDecoder.booleanSegment, PathEncoder.booleanSegment, UriTemplateEncoder.Simple.segment)
    
    def enumSegment[E <: Enumeration](e:E) = factory.create(PathDecoder.enumSegment(e), PathEncoder.enumSegment[E], UriTemplateEncoder.Simple.segment)
  }

  object Param {
    private val factory = PathConverter.factory[CanAddParam, CanHaveParamsAsPrefix]

    val string = factory.create(PathDecoder.stringParam, PathEncoder.stringParam, UriTemplateEncoder.Simple.param)
    val int = factory.create(PathDecoder.intParam, PathEncoder.intParam, UriTemplateEncoder.Simple.param)
    val long = factory.create(PathDecoder.longParam, PathEncoder.longParam, UriTemplateEncoder.Simple.param)
    val boolean = factory.create(PathDecoder.booleanParam, PathEncoder.booleanParam, UriTemplateEncoder.Simple.param)

    def string(parName: String): PathConverter[String, String, String, CanAddParam, CanHaveParamsAsPrefix] =
      factory.create(
        PathDecoder.stringParamValue(parName),
        PathEncoder.stringParamValue(parName),
        UriTemplateEncoder.Simple.paramNamed(parName))

    def int(parName: String): PathConverter[Int, Int, String, CanAddParam, CanHaveParamsAsPrefix] =
      factory.create(
        PathDecoder.intParamValue(parName),
        PathEncoder.intParamValue(parName),
        UriTemplateEncoder.Simple.paramNamed(parName))

    def long(parName: String): PathConverter[Long, Long, String, CanAddParam, CanHaveParamsAsPrefix] =
      factory.create(
        PathDecoder.longParamValue(parName),
        PathEncoder.longParamValue(parName),
        UriTemplateEncoder.Simple.paramNamed(parName))

    def boolean(parName: String): PathConverter[Boolean, Boolean, String, CanAddParam, CanHaveParamsAsPrefix] =
      factory.create(
        PathDecoder.booleanParamValue(parName),
        PathEncoder.booleanParamValue(parName),
        UriTemplateEncoder.Simple.paramNamed(parName))

        
    def enum[E <: Enumeration](e:E, parName:String) = factory.create(PathDecoder.enumParamValue(e)(parName), PathEncoder.enumParamValue[E](parName), UriTemplateEncoder.Simple.paramNamed(parName))    
  }

}

class PathConverterFactory[A <: CanAddAspect, P <: CanHavePrefixAspect] {
  def create[D, E, UT](d: PathDecoder[D], e: PathEncoder[E], ute: UriTemplateEncoder[UT]): PathConverter[D, E, UT, A, P] = {
    PathConverter[D, E, UT, A, P](d, e, ute)
  }
}