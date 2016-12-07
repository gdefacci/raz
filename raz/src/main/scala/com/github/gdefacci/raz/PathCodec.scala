package com.github.gdefacci.raz

import scalaz.{ -\/, \/, \/- }

import scala.language.implicitConversions

case class PathCodec[TD, TE, S <: PathPosition, E <: PathPosition](
    fullPathCodec:Option[PathCodec[TD, TE, S, E]],
    decoder: PathDecoder[TD, S, E], encoder: PathEncoder[TE, S, E])
    extends PathDecoderMixin[TD] with PathEncoderMixin[TE, S, E] with Api.PathCodec[TD, TE] {

  lazy val fullPath = fullPathCodec.getOrElse(this)
  
  def decode(path: Path): Throwable \/ MatchResult[TD] = decoder.decode(path)
  def encode(t: TE): TPath[S, E] = encoder.encode(t)

  type DecoderType[T1] = PathCodec[T1, TE, S, E]
  protected def createDecoder[T1](f: Path => Throwable \/ MatchResult[T1]) = new PathCodec[T1, TE, S, E](fullPathCodec.map(_.createDecoder(f)), PathDecoder(f), encoder)

  type EncoderType[T1] = PathCodec[TD, T1, S, E]
  
  override def contramap[T1](f: T1 => TE) = createEncoder(f.andThen(encode(_)))
  protected def createEncoder[T1](f: T1 => TPath[S, E]):PathCodec[TD,T1,S,E] = PathCodec(fullPathCodec.map(_.createEncoder(f)), decoder, PathEncoder(f))

//  def append[S1 <: PathPosition, E1 <: PathPosition](suffix: TPath[S1, E1])(implicit prefixPathAppender:PathAppender[S,S], suffixPathAppender: PathAppender[E, S1]): PathCodec[TD, TE, S, E1] =
//    PathCodec(fullPathCodec.map(_.append(suffix)), decoder.append(suffix), encoder.append(suffix))
//
//  def prepend[S2 <: PathPosition, E2 <: PathPosition](prefix: TPath[S2, E2])(implicit prefixPathAppender: PathAppender[E2, S], suffixPathAppender:PathAppender[E,E]): PathCodec[TD, TE, S2, E] =
//    PathCodec(fullPathCodec.map(_.prepend(prefix)), decoder.prepend(prefix), encoder.prepend(prefix))

  def append[S1 <: PathPosition, E1 <: PathPosition](suffix: TPath[S1, E1])(implicit suffixPathAppender: PathAppender[E, S1]): PathCodec[TD, TE, S, E1] =
    PathCodec(fullPathCodec.map(_.append(suffix)), 
        RightPathDecoder(decoder, suffix),
        RightPathEncoder(encoder, suffix))

  def prepend[S2 <: PathPosition, E2 <: PathPosition](prefix: TPath[S2, E2])(implicit prefixPathAppender: PathAppender[E2, S]): PathCodec[TD, TE, S2, E] =
    PathCodec(fullPathCodec.map(_.prepend(prefix)), 
        LeftPathDecoder(prefix, decoder), 
        LeftPathEncoder(prefix, encoder))

  private[raz] def decoderAt(p: Path) = PathCodec[TD, TE, S, E](fullPathCodec.orElse(Some(this)), decoder.decoderAt(p), encoder)
  
  def caseMap[C](mf:TD => C)(cf:C => Option[TE]):PathCodec[C,C,S,E] = 
    PathCodec[C,C,S,E](fullPathCodec.map( fp => fp.caseMap(mf)(cf)), decoder.map(mf), encoder.contramap( (a:C) => cf(a).get ))
  
}

import shapeless._

// case class HPathCodec[H <: HList, HR <: HList, S <: PathPosition, E <: PathPosition](decoder: HPathDecoder[H, HR, S, E], encoder: HPathEncoder[H, HR, S, E])
// 
// object HPathCodec {
// 
//   def apply[H <: HList, HR <: HList, S <: PathPosition, E <: PathPosition](implicit enc: HPathEncoder[H, HR, S, E], dec: HPathDecoder[H, HR, S, E]): HPathCodec[H, HR, S, E] = {
//     HPathCodec[H, HR, S, E](dec, enc)
//   }
// 
// }

object PathCodec {
  
  implicit def toPathOps[TD, TE, S <: PathPosition, E <: PathPosition](p:PathCodec[TD, TE, S,E]) = new PathOps(p)

  def apply[TD, TE, S <: PathPosition, E <: PathPosition](decoder: PathDecoder[TD, S, E], encoder: PathEncoder[TE, S, E]) =
    new PathCodec[TD, TE, S, E](None, decoder, encoder)
    
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