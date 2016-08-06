package org.obl.raz

import shapeless._
import shapeless.ops.hlist.Tupler
import scala.language.implicitConversions

sealed trait UriTemplateEncoder[T, S <: PathPosition, E <: PathPosition] extends Api.UriTemplateEncoder[T] {

  type UriTemplateEncoderType[T] = UriTemplateEncoder[T, S, E]

  def contramap[T1](f: T1 => T): UriTemplateEncoder[T1, S, E] = UriTemplateEncoder(f.andThen(encodeUriTemplate(_)))

  def encodeUriTemplate(t: T): TUriTemplate[S, E]

  def append[S1 <: PathPosition, E1 <: PathPosition](suffix: TPath[S1, E1])(implicit pathAppender: PathAppender[E, S1]): UriTemplateEncoder[T, S, E1] =
    UriTemplateEncoder[T, S, E1] { t =>
      val ut = encodeUriTemplate(t)
      new TUriTemplate[S, E1](
          ut.scheme.orElse(suffix.scheme),
          ut.authority.orElse(suffix.authority),
        ut.segments ++ suffix.segments.map(UriTemplate.Segment(_)),
        ut.params ++ suffix.params.map(UriTemplate.Param(_)),
        ut.fragment.orElse(suffix.fragment.map(UriTemplate.Fragment(_))))
    }

  def prepend[S2 <: PathPosition, E2 <: PathPosition](prefix: TPath[S2, E2])(implicit pathAppender: PathAppender[E2, S]): UriTemplateEncoder[T, S2, E] =
    UriTemplateEncoder[T, S2, E] { t =>
      val ut = encodeUriTemplate(t)
      new TUriTemplate[S2, E](
          prefix.scheme.orElse(ut.scheme),
          prefix.authority.orElse(ut.authority),
        prefix.segments.map(UriTemplate.Segment(_)) ++ ut.segments,
        prefix.params.map(UriTemplate.Param(_)) ++ ut.params,
        prefix.fragment.map(UriTemplate.Fragment(_)).orElse(ut.fragment))
    }

  def uriTemplateEncoderAt(uriTemplateEncoderPrefix: Path) = UriTemplateEncoder[T,S,E] { v =>
    val ut = this.encodeUriTemplate(v) 
    val ut0 = DecodeUtils.subtract(ut, uriTemplateEncoderPrefix).getOrElse(ut)
    new TUriTemplate[S, E]( ut0.scheme, ut0.authority, ut0.segments, ut0.params, ut0.fragment)
  }

    
}

object UriTemplateEncoder {

  def apply[T, S <: PathPosition, E <: PathPosition](f: T => TUriTemplate[S, E]) = new UriTemplateEncoder[T, S, E] {
    def encodeUriTemplate(t: T): TUriTemplate[S, E] = f(t)
  }

  implicit def apply[H <: HList, TUP, S <: PathPosition, E <: PathPosition](h: H)(implicit tue:ToUriTemplateEncoder[H,TUP,S,E]):UriTemplateEncoder[TUP,S,E] = tue(h)

//  implicit final class UriTemplateEncoderPathEncoderWrapper[T, S <: PathPosition, E <: PathPosition](enc: PathEncoder[T, S, E]) extends UriTemplateEncoder[String, S, E] {
//
//    def encodeUriTemplate(t: String): TUriTemplate[S, E] = HUriTemplateEncoder.encoderToUriTemplate(enc, t)
//    
//  }

}

trait ToUriTemplateEncoder[H, T, S <: PathPosition, E <: PathPosition] {

  def apply(h: H): UriTemplateEncoder[T, S, E]

}

object ToUriTemplateEncoder {

  implicit def htuple[H <: HList, HR <: HList, TUP, S <: PathPosition, E <: PathPosition](
    implicit hr: HUriTemplateEncoder[H, HR, S, E],
    tupler: Tupler.Aux[HR, TUP],
    gen: Generic.Aux[TUP, HR]): ToUriTemplateEncoder[H, TUP, S, E] =
      new ToUriTemplateEncoder[H, TUP, S, E] {
        def apply(h: H) = {
          val hrEnc = hr.apply(h)
          UriTemplateEncoder { (t:TUP) =>
            val h1 = gen.to(t)
            hrEnc.apply(h1)
          }
        }
      }

//  implicit def htuple1PathEncoder[T, S <: PathPosition, E <: PathPosition] =
//    new ToUriTemplateEncoder[PathEncoder[T,S,E] :: HNil, String, S, E] {
//        def apply(h: PathEncoder[T,S,E] :: HNil):UriTemplateEncoder[String,S,E] = new UriTemplateEncoder.UriTemplateEncoderPathEncoderWrapper[T,S,E](h.head)
//      }
  
  implicit def htuple1PathConverter[TD,TE,UT, S <: PathPosition, E <: PathPosition] =
    new ToUriTemplateEncoder[PathConverter[TD,TE,UT,S,E] :: HNil, UT, S, E] {
        def apply(h: PathConverter[TD,TE,UT,S,E] :: HNil):UriTemplateEncoder[UT,S,E] = h.head.uriTemplateEncoder
      }

}

trait HUriTemplateEncoder[H <: HList, HR <: HList, S <: PathPosition, E <: PathPosition] {
  def apply(h: H): HR => TUriTemplate[S, E]
}

object HUriTemplateEncoder {
  def apply[H <: HList, HR <: HList, S <: PathPosition, E <: PathPosition](f: H => HR => TUriTemplate[S, E]) =
    new HUriTemplateEncoder[H, HR, S, E] {
      def apply(h: H): HR => TUriTemplate[S, E] = f(h)
    }

  def placeholder(k: UriTemplate.Kind, nm: String): UriTemplate = k match {
    case UriTemplate.Segment => UriTemplate / UriTemplate.PlaceHolder(nm)
    case UriTemplate.Param => UriTemplate.fragment(UriTemplate.PlaceHolder(nm))
    case UriTemplate.ParamWithName(name) => UriTemplate && (name, UriTemplate.PlaceHolder(nm))
    case UriTemplate.Fragment => UriTemplate &# UriTemplate.PlaceHolder(nm)
  }

  implicit def hnilConverterHUriTemplateEncoder[TD, T, UT, S <: PathPosition, E <: PathPosition]: HUriTemplateEncoder[PathConverter[TD, T, UT, S, E] :: HNil, UT :: HNil, S, E] =
    HUriTemplateEncoder[PathConverter[TD, T, UT, S, E] :: HNil, UT :: HNil, S, E](h => h1 => h.head.uriTemplateEncoder.encodeUriTemplate(h1.head))

  implicit def hconsConverterHUriTemplateEncoder[H <: HList, HR <: HList, TD, T, UT, S <: PathPosition, E <: PathPosition, S1 <: PathPosition, E1 <: PathPosition](implicit hr: HUriTemplateEncoder[H, HR, S1, E1], pathAppender: PathAppender[E, S1]): HUriTemplateEncoder[PathConverter[TD, T, UT, S, E] :: H, UT :: HR, S, E1] =
    HUriTemplateEncoder[PathConverter[TD, T, UT, S, E] :: H, UT :: HR, S, E1] { h =>
      h1 =>
        val tp1 = h.head.uriTemplateEncoder.encodeUriTemplate(h1.head)
        val tp2 = hr.apply(h.tail).apply(h1.tail)
        tp1.append(tp2)
    }

}
