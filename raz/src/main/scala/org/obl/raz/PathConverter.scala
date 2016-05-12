package org.obl.raz

import shapeless._
import shapeless.ops.hlist.Tupler
import scalaz.{ -\/, \/, \/- }
import scala.language.implicitConversions

case class PathConverter[TD, TE, TU, S <: PathPosition, E <: PathPosition](fullPathConverter:Option[PathConverter[TD,TE,TU,S,E]], decoder: PathDecoder[TD, S, E], encoder: PathEncoder[TE, S, E], uriTemplateEncoder: UriTemplateEncoder[TU, S, E])
    extends PathEncoderMixin[TE, S, E] with PathDecoderMixin[TD] with Api.PathConverter[TD, TE, TU] {
  
  lazy val pathConverter = this
  lazy val fullPath = fullPathConverter.getOrElse(this)
  
  private[raz] lazy val kind = encoder.kind
  def decode(path: Path): Throwable \/ MatchResult[TD] = decoder.decode(path)
  def encode(t: TE): TPath[S, E] = encoder.encode(t)
  def encodeUriTemplate(t: TU): UriTemplate = uriTemplateEncoder.encodeUriTemplate(t)

  type DecoderType[T1] = PathConverter[T1, TE, TU, S, E]
  protected def createDecoder[T1](f: Path => Throwable \/ MatchResult[T1]):PathConverter[T1, TE, TU, S, E] = 
    new PathConverter[T1, TE, TU, S, E](fullPathConverter.map(_.createDecoder(f)), PathDecoder(f), encoder, uriTemplateEncoder)

  type EncoderType[T1] = PathConverter[TD, T1, TU, S, E]
  protected def createEncoder[T1](kind: org.obl.raz.UriTemplate.Kind, f: T1 => TPath[S, E]):PathConverter[TD,T1,TU,S,E] = PathConverter(fullPathConverter.map(_.createEncoder[T1](kind, f)), decoder, PathEncoder(kind, f), uriTemplateEncoder)

  def append[S1 <: PathPosition, E1 <: PathPosition](suffix: TPath[S1, E1])(implicit pathAppender: PathAppender[E, S1]): PathConverter[TD, TE, TU, S, E1] =
    PathConverter(fullPathConverter.map(_.append(suffix)), decoder.append(suffix), encoder.append(suffix), uriTemplateEncoder.append(suffix))

  def prepend[S2 <: PathPosition, E2 <: PathPosition](prefix: TPath[S2, E2])(implicit pathAppender: PathAppender[E2, S]): PathConverter[TD, TE, TU, S2, E] =
    PathConverter(fullPathConverter.map(_.prepend(prefix)), decoder.prepend(prefix), encoder.prepend(prefix), uriTemplateEncoder.prepend(prefix))

  object UriTemplate {

    def contramap[T1](f: T1 => TU): PathConverter[TD, TE, T1, S, E] = PathConverter(fullPathConverter.map(_.UriTemplate.contramap(f)), decoder, encoder, uriTemplateEncoder.contramap(f))

  }

  def decoderAt(p: Path) = PathConverter[TD,TE,TU,S,E](fullPathConverter.orElse(Some(this)), decoder.decoderAt(p), encoder, uriTemplateEncoder)
  
  def uriTemplateEncoderAt(p: Path) = PathConverter[TD,TE,TU,S,E](fullPathConverter.orElse(Some(this)), decoder, encoder, uriTemplateEncoder.uriTemplateEncoderAt(p))
  
  def caseMap[C](mf:TD => C)(cf:C => Option[TE]):PathConverter[C,C,TU,S,E] = 
    PathConverter[C,C,TU,S,E](fullPathConverter.map(c => c.caseMap[C](mf)(cf)), decoder.map(mf), encoder.contramap( (a:C) => cf(a).get ), uriTemplateEncoder)
  
}

/*
case class HPathConverter[H <: HList, HR <: HList, S <: PathPosition, E <: PathPosition](decoder: HPathDecoder[H, HR, S, E], encoder: HPathEncoder[H, HR, S, E], uriTemplateEncoder: HUriTemplateEncoder[H, HR, S, E])

object HPathConverter {

  def apply[H <: HList, HR <: HList, S <: PathPosition, E <: PathPosition](implicit enc: HPathEncoder[H, HR, S, E], dec: HPathDecoder[H, HR, S, E], ut: HUriTemplateEncoder[H, HR, S, E]): HPathConverter[H, HR, S, E] = {
    HPathConverter[H, HR, S, E](dec, enc, ut)
  }

}
*/
object PathConverter {
  
  def apply[TD, TE, TU, S <: PathPosition, E <: PathPosition](decoder: PathDecoder[TD, S, E], encoder: PathEncoder[TE, S, E], uriTemplateEncoder: UriTemplateEncoder[TU, S, E]) =
    new PathConverter[TD, TE, TU, S, E](None, decoder, encoder, uriTemplateEncoder)
    

  implicit def hTupleConverter1[H <: HList, TD,TE, UT, S <: PathPosition, E <: PathPosition](h:H)
    (implicit tcn:ToPathConverter[H,TD,TE,UT,S,E]): PathConverter[TD, TE, UT, S, E] = tcn(h)

  implicit def toPathConverterBuilder[TD, TE, UT, S <: PathPosition, E <: PathPosition](pe: PathConverter[TD, TE, UT, S, E]): PathConverterBuilder[PathConverter[TD, TE, UT, S, E] :: HNil, HNil, S, TD, TE, UT, S, E] =
    PathConverterBuilder[PathConverter[TD, TE, UT, S, E] :: HNil, HNil, S, TD, TE, UT, S, E](pe :: HNil, HNil, pe)

  object Segment {

    private val uriTemplateEncoder = UriTemplateEncoder[String, PathPosition.Segment, PathPosition.Segment](t => UriTemplate / UriTemplate.PlaceHolder(t))

    val string = PathConverter(PathDecoder.Segment.string, PathEncoder.Segment.string, uriTemplateEncoder)
    val int = PathConverter(PathDecoder.Segment.int, PathEncoder.Segment.int, uriTemplateEncoder)
    val long = PathConverter(PathDecoder.Segment.long, PathEncoder.Segment.long, uriTemplateEncoder)
    val boolean = PathConverter(PathDecoder.Segment.boolean, PathEncoder.Segment.boolean, uriTemplateEncoder)

  }

  case class Param(name: String) {

    private val uriTemplateEncoder = UriTemplateEncoder[String, PathPosition.Param, PathPosition.Param](t => UriTemplate && (name, UriTemplate.PlaceHolder(t)))

    val string = PathConverter(PathDecoder.Param(name).string, PathEncoder.Param(name).string, uriTemplateEncoder)
    val int = PathConverter(PathDecoder.Param(name).int, PathEncoder.Param(name).int, uriTemplateEncoder)
    val boolean = PathConverter(PathDecoder.Param(name).boolean, PathEncoder.Param(name).boolean, uriTemplateEncoder)

  }

  object Fragment {

    private val uriTemplateEncoder = UriTemplateEncoder[String, PathPosition.Fragment, PathPosition.Fragment](t => UriTemplate &# UriTemplate.PlaceHolder(t))

    val string = PathConverter(PathDecoder.Fragment.string, PathEncoder.Fragment.string, uriTemplateEncoder)
    val int = PathConverter(PathDecoder.Fragment.int, PathEncoder.Fragment.int, uriTemplateEncoder)
    val boolean = PathConverter(PathDecoder.Fragment.boolean, PathEncoder.Fragment.boolean, uriTemplateEncoder)

  }

}

trait ToPathConverter[H <: HList, TD, TE, TU, S <: PathPosition, E <: PathPosition] {
  def apply(h: H): PathConverter[TD, TE, TU, S, E]
}

object ToPathConverter {
  
   implicit def htuple[H <: HList, TD, TE, UT, S <: PathPosition, E <: PathPosition]
    (implicit
        topd:ToPathDecoder[H, TD, S,E],
        tope:ToPathEncoder[H, TE, S,E],
        toue:ToUriTemplateEncoder[H, UT, S,E]) =
    new ToPathConverter[H, TD, TE, UT, S, E] {
      def apply(h: H) = PathConverter[TD, TE, UT, S, E](topd(h), tope(h), toue(h))
    }
  
  implicit def htuple1[TD, TE, TU, S <: PathPosition, E <: PathPosition] =
    new ToPathConverter[PathConverter[TD, TE, TU, S, E] :: HNil, TD, TE, TU, S, E] {
      def apply(h: PathConverter[TD, TE, TU, S, E] :: HNil): PathConverter[TD, TE, TU, S, E] = {
        h.head
      }
    }
}