package org.obl.raz

import shapeless._
import shapeless.ops.hlist.Tupler
import scalaz.{ -\/, \/, \/- }
import scala.language.implicitConversions

case class PathConverter[TD, TE, TU, S <: PathPosition, E <: PathPosition](
    fullPathConverter:Option[PathConverter[TD,TE,TU,S,E]], 
    decoder: PathDecoder[TD, S, E], 
    encoder: PathEncoder[TE, S, E], 
    uriTemplateEncoder: UriTemplateEncoder[TU, S, E]) extends PathEncoderMixin[TE, S, E] with PathDecoderMixin[TD] with Api.PathConverter[TD, TE, TU] {
  
  lazy val fullPath = fullPathConverter.getOrElse(this)
  
  def decode(path: Path): Throwable \/ MatchResult[TD] = decoder.decode(path)
  def encode(t: TE): TPath[S, E] = encoder.encode(t)
  def encodeUriTemplate(t: TU): UriTemplate = uriTemplateEncoder.encodeUriTemplate(t)

  type DecoderType[T1] = PathConverter[T1, TE, TU, S, E]
  protected def createDecoder[T1](f: Path => Throwable \/ MatchResult[T1]):PathConverter[T1, TE, TU, S, E] = 
    new PathConverter[T1, TE, TU, S, E](fullPathConverter.map(_.createDecoder(f)), PathDecoder(f), encoder, uriTemplateEncoder)

  type EncoderType[T1] = PathConverter[TD, T1, TU, S, E]
  protected def createEncoder[T1](f: T1 => TPath[S, E]):PathConverter[TD,T1,TU,S,E] = PathConverter(fullPathConverter.map(_.createEncoder[T1](f)), decoder, PathEncoder(f), uriTemplateEncoder)

  override def contramap[T1](f: T1 => TE) = createEncoder(f.andThen(encode(_)))
  
  def append[S1 <: PathPosition, E1 <: PathPosition](suffix: TPath[S1, E1])(implicit suffixPathAppender: PathAppender[E, S1]): PathConverter[TD, TE, TU, S, E1] =
    PathConverter(fullPathConverter.map(_.append(suffix)), 
        RightPathDecoder(decoder, suffix),
        RightPathEncoder(encoder, suffix), 
        uriTemplateEncoder.append(suffix))

  def prepend[S2 <: PathPosition, E2 <: PathPosition](prefix: TPath[S2, E2])(implicit prefixPathAppender: PathAppender[E2, S]): PathConverter[TD, TE, TU, S2, E] =
    PathConverter(fullPathConverter.map(_.prepend(prefix)), 
        LeftPathDecoder(prefix, decoder), 
        LeftPathEncoder(prefix, encoder), 
        uriTemplateEncoder.prepend(prefix))

  object UriTemplate {

    def contramap[T1](f: T1 => TU): PathConverter[TD, TE, T1, S, E] = PathConverter(fullPathConverter.map(_.UriTemplate.contramap(f)), decoder, encoder, uriTemplateEncoder.contramap(f))

  }

  private[raz] def decoderAt(p: Path) = PathConverter[TD,TE,TU,S,E](fullPathConverter.orElse(Some(this)), decoder.decoderAt(p), encoder, uriTemplateEncoder)
  
  def uriTemplateEncoderAt(p: Path) = PathConverter[TD,TE,TU,S,E](fullPathConverter.orElse(Some(this)), decoder, encoder, uriTemplateEncoder.uriTemplateEncoderAt(p))
  
  def caseMap[C](mf:TD => C)(cf:C => Option[TE]):PathConverter[C,C,TU,S,E] = 
    PathConverter[C,C,TU,S,E](fullPathConverter.map(c => c.caseMap[C](mf)(cf)), decoder.map(mf), encoder.contramap( (a:C) => cf(a).get ), uriTemplateEncoder)

  def toCodec:PathCodec[TD, TE, S, E] = new PathCodec[TD, TE, S, E](fullPathConverter.map(_.toCodec), decoder, encoder)  
}

object PathConverter {

  implicit def toPathOps[TD, TE, TU, S <: PathPosition, E <: PathPosition](p:PathConverter[TD, TE, TU,S,E]) = new PathOps(p)
  
//  def shift[TD, TE, TU](pc:PathConverter[TD,TE,TU,PathPosition.Absolute,PathPosition.Segment], 
//      prefix: TPath[PathPosition.Absolute,PathPosition.Segment], 
//      decodeAtPath:TPath[PathPosition.Segment,PathPosition.Segment], 
//      uriTemplateAtPath:TPath[PathPosition.Absolute,PathPosition.Segment]):PathConverter[TD,TE,TU,PathPosition.Absolute,PathPosition.Segment] = {
//    pc.prepend(prefix).decoderAt(decodeAtPath).uriTemplateEncoderAt(uriTemplateAtPath)
//  }
  
  def apply[TD, TE, TU, S <: PathPosition, E <: PathPosition](decoder: PathDecoder[TD, S, E], encoder: PathEncoder[TE, S, E], uriTemplateEncoder: UriTemplateEncoder[TU, S, E]) =
    new PathConverter[TD, TE, TU, S, E](None, decoder, encoder, uriTemplateEncoder)
    
  object Segment {

    private val uriTemplateEncoder = UriTemplateEncoder[String, PathPosition.Segment, PathPosition.Segment](t => UriTemplate / UriTemplate.PlaceHolder(t))

    val string = PathConverter(PathDecoder.Segment.string, PathEncoder.Segment.string, uriTemplateEncoder)
    val int = PathConverter(PathDecoder.Segment.int, PathEncoder.Segment.int, uriTemplateEncoder)
    val long = PathConverter(PathDecoder.Segment.long, PathEncoder.Segment.long, uriTemplateEncoder)
    val boolean = PathConverter(PathDecoder.Segment.boolean, PathEncoder.Segment.boolean, uriTemplateEncoder)

  }

  case class Param(name: String) {

    private val uriTemplateEncoder = UriTemplateEncoder[String, PathPosition.Param, PathPosition.Param](t => UriTemplate.param(name, UriTemplate.PlaceHolder(t)))

    val string = PathConverter(PathDecoder.Param(name).string, PathEncoder.Param(name).string, uriTemplateEncoder)
    val int = PathConverter(PathDecoder.Param(name).int, PathEncoder.Param(name).int, uriTemplateEncoder)
    val boolean = PathConverter(PathDecoder.Param(name).boolean, PathEncoder.Param(name).boolean, uriTemplateEncoder)

  }

  object Fragment {

    private val uriTemplateEncoder = UriTemplateEncoder[String, PathPosition.Fragment, PathPosition.Fragment](t => UriTemplate.fragment(UriTemplate.PlaceHolder(t)))

    val string = PathConverter(PathDecoder.Fragment.string, PathEncoder.Fragment.string, uriTemplateEncoder)
    val int = PathConverter(PathDecoder.Fragment.int, PathEncoder.Fragment.int, uriTemplateEncoder)
    val boolean = PathConverter(PathDecoder.Fragment.boolean, PathEncoder.Fragment.boolean, uriTemplateEncoder)

  }

}