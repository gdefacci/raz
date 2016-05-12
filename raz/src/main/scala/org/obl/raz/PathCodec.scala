package org.obl.raz

import shapeless._
import shapeless.ops.hlist.Tupler
import scalaz.{ -\/, \/, \/- }

import scala.language.implicitConversions

case class PathCodec[TD, TE, S <: PathPosition, E <: PathPosition](
    fullPathCodec:Option[PathCodec[TD, TE, S, E]],
    decoder: PathDecoder[TD, S, E], encoder: PathEncoder[TE, S, E])
    extends PathDecoderMixin[TD] with PathEncoderMixin[TE, S, E] with Api.PathCodec[TD, TE] {

  lazy val pathCodec = this
  lazy val fullPath = fullPathCodec.getOrElse(this)
  
  private[raz] lazy val kind = encoder.kind
  def decode(path: Path): Throwable \/ MatchResult[TD] = decoder.decode(path)
  def encode(t: TE): TPath[S, E] = encoder.encode(t)

  type DecoderType[T1] = PathCodec[T1, TE, S, E]
  protected def createDecoder[T1](f: Path => Throwable \/ MatchResult[T1]) = new PathCodec[T1, TE, S, E](fullPathCodec.map(_.createDecoder(f)), PathDecoder(f), encoder)

  type EncoderType[T1] = PathCodec[TD, T1, S, E]
  protected def createEncoder[T1](kind: UriTemplate.Kind, f: T1 => TPath[S, E]) = PathCodec(fullPathCodec.map(_.createEncoder(kind, f)), decoder, PathEncoder(kind, f))

  def append[S1 <: PathPosition, E1 <: PathPosition](suffix: TPath[S1, E1])(implicit pathAppender: PathAppender[E, S1]): PathCodec[TD, TE, S, E1] =
    PathCodec(fullPathCodec.map(_.append(suffix)), decoder.append(suffix), encoder.append(suffix))

  def prepend[S2 <: PathPosition, E2 <: PathPosition](prefix: TPath[S2, E2])(implicit pathAppender: PathAppender[E2, S]): PathCodec[TD, TE, S2, E] =
    PathCodec(fullPathCodec.map(_.prepend(prefix)), decoder.prepend(prefix), encoder.prepend(prefix))

  def decoderAt(p: Path) = PathCodec[TD, TE, S, E](fullPathCodec.orElse(Some(this)), decoder.decoderAt(p), encoder)
  
  def caseMap[C](mf:TD => C)(cf:C => Option[TE]):PathCodec[C,C,S,E] = 
    PathCodec[C,C,S,E](fullPathCodec.map( fp => fp.caseMap(mf)(cf)), decoder.map(mf), encoder.contramap( (a:C) => cf(a).get ))
  
}

case class HPathCodec[H <: HList, HR <: HList, S <: PathPosition, E <: PathPosition](decoder: HPathDecoder[H, HR, S, E], encoder: HPathEncoder[H, HR, S, E])

object HPathCodec {

  def apply[H <: HList, HR <: HList, S <: PathPosition, E <: PathPosition](implicit enc: HPathEncoder[H, HR, S, E], dec: HPathDecoder[H, HR, S, E]): HPathCodec[H, HR, S, E] = {
    HPathCodec[H, HR, S, E](dec, enc)
  }

}

object PathCodec {

  def apply[TD, TE, S <: PathPosition, E <: PathPosition](decoder: PathDecoder[TD, S, E], encoder: PathEncoder[TE, S, E]) =
    new PathCodec[TD, TE, S, E](None, decoder, encoder)
    
  implicit def hTupleCodec[H <: HList, TD,TE, S <: PathPosition, E <: PathPosition](h: H)
    (implicit tpc:ToPathCodec[H,TD,TE,S,E]): PathCodec[TD, TE, S, E] = tpc(h)

  implicit def toPathCodecBuilder[TD, TE, S <: PathPosition, E <: PathPosition](pe: PathCodec[TD, TE, S, E]): PathCodecBuilder[PathCodec[TD, TE, S, E] :: HNil, HNil, S, TD, TE, S, E] =
    PathCodecBuilder[PathCodec[TD, TE, S, E] :: HNil, HNil, S, TD, TE, S, E](pe :: HNil, HNil, pe)

  object Segment {

    val string = PathCodec(PathDecoder.Segment.string, PathEncoder.Segment.string)
    val int = PathCodec(PathDecoder.Segment.int, PathEncoder.Segment.int)
    val boolean = PathCodec(PathDecoder.Segment.boolean, PathEncoder.Segment.boolean)

  }

  case class Param(name: String) {

    val string = PathCodec(PathDecoder.Param(name).string, PathEncoder.Param(name).string)
    val int = PathCodec(PathDecoder.Param(name).int, PathEncoder.Param(name).int)
    val boolean = PathCodec(PathDecoder.Param(name).boolean, PathEncoder.Param(name).boolean)

  }

  object Fragment {

    val string = PathCodec(PathDecoder.Fragment.string, PathEncoder.Fragment.string)
    val int = PathCodec(PathDecoder.Fragment.int, PathEncoder.Fragment.int)
    val boolean = PathCodec(PathDecoder.Fragment.boolean, PathEncoder.Fragment.boolean)

  }

}

trait ToPathCodec[H <: HList, TD, TE, S <: PathPosition, E <: PathPosition] {
  def apply(h: H): PathCodec[TD, TE, S, E]
}

object ToPathCodec {
  
  private def fromPathConverter[TD, TE, TU, S <: PathPosition, E <: PathPosition](pcnv:PathConverter[TD,TE,TU,S,E]):PathCodec[TD,TE,S,E] = {
    PathCodec(pcnv.fullPathConverter.map( cnv => fromPathConverter(cnv)), pcnv.decoder, pcnv.encoder )
  }

  implicit def htupleConverter1[TD, TE, TU, S <: PathPosition, E <: PathPosition] =
    new ToPathCodec[PathConverter[TD, TE, TU, S, E] :: HNil, TD, TE, S, E] {
      def apply(h: PathConverter[TD, TE, TU, S, E] :: HNil): PathCodec[TD, TE, S, E] = {
        fromPathConverter(h.head)
      }
    }
  
  implicit def hTupleCodec[H <: HList, TD, TE, S <: PathPosition, E <: PathPosition](
      implicit 
        topd:ToPathDecoder[H,TD,S,E], 
        tope:ToPathEncoder[H,TE,S,E]) = 
        new ToPathCodec[H, TD, TE, S, E] {
          def apply(h: H) = PathCodec(topd(h), tope(h))
        }

  implicit def htupleCodec1[TD, TE, S <: PathPosition, E <: PathPosition] =
    new ToPathCodec[PathCodec[TD, TE, S, E] :: HNil, TD, TE, S, E] {
      def apply(h: PathCodec[TD, TE, S, E] :: HNil): PathCodec[TD, TE, S, E] = {
        h.head
      }
    }

}
