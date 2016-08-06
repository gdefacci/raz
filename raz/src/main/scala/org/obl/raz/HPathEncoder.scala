package org.obl.raz

import shapeless._
import scalaz.{ -\/, \/, \/- }

trait HPathEncoder[H <: HList, HR <: HList, S <: PathPosition, E <: PathPosition] {
  def apply(h: H): HR => TPath[S, E]
}

object HPathEncoder {
  private def apply[H <: HList, HR <: HList, S <: PathPosition, E <: PathPosition](f: H => HR => TPath[S, E]) = new HPathEncoder[H, HR, S, E] {
    def apply(h: H): HR => TPath[S, E] = f(h)
  }

  implicit def hnilHPathEncoder[T, S <: PathPosition, E <: PathPosition]: HPathEncoder[PathEncoder[T, S, E] :: HNil, T :: HNil, S, E] =
    HPathEncoder[PathEncoder[T, S, E] :: HNil, T :: HNil, S, E](h => h1 => h.head.encode(h1.head))

  implicit def hconsHPathEncoder[H <: HList, HR <: HList, T, S <: PathPosition, E <: PathPosition, S1 <: PathPosition, E1 <: PathPosition](
      implicit hr: HPathEncoder[H, HR, S1, E1], 
      pathAppender: PathAppender[E, S1]): HPathEncoder[PathEncoder[T, S, E] :: H, T :: HR, S, E1] =
    HPathEncoder[PathEncoder[T, S, E] :: H, T :: HR, S, E1]({ h =>
      h1 =>
        val tp1 = h.head.encode(h1.head)
        val tp2 = hr.apply(h.tail).apply(h1.tail)
        tp1.append(tp2)
    })

  implicit def hnilHPathCodecEncoder[TD, TE, S <: PathPosition, E <: PathPosition]: HPathEncoder[PathCodec[TD, TE, S, E] :: HNil, TE :: HNil, S, E] =
    HPathEncoder[PathCodec[TD, TE, S, E] :: HNil, TE :: HNil, S, E](h => h1 => h.head.encoder.encode(h1.head))

  implicit def hconsHPathCodecEncoder[H <: HList, HR <: HList, TD, TE, S <: PathPosition, E <: PathPosition, S1 <: PathPosition, E1 <: PathPosition]
    (implicit hr: HPathEncoder[H, HR, S1, E1], pathAppender: PathAppender[E, S1]): HPathEncoder[PathCodec[TD, TE, S, E] :: H, TE :: HR, S, E1] =
    HPathEncoder[PathCodec[TD, TE, S, E] :: H, TE :: HR, S, E1] { h =>
      h1 =>
        val tp1 = h.head.encoder.encode(h1.head)
        val tp2 = hr.apply(h.tail).apply(h1.tail)
        tp1.append(tp2)
    }

  implicit def hnilHPathConverterEncoder[TD, TE, UT, S <: PathPosition, E <: PathPosition]: HPathEncoder[PathConverter[TD, TE, UT, S, E] :: HNil, TE :: HNil, S, E] =
    HPathEncoder[PathConverter[TD, TE, UT, S, E] :: HNil, TE :: HNil, S, E](h => h1 => h.head.encoder.encode(h1.head))

  implicit def hconsHPathConverterEncoder[H <: HList, HR <: HList, TD, TE, UT, S <: PathPosition, E <: PathPosition, S1 <: PathPosition, E1 <: PathPosition]
    (implicit hr: HPathEncoder[H, HR, S1, E1], pathAppender: PathAppender[E, S1]): HPathEncoder[PathConverter[TD, TE, UT, S, E] :: H, TE :: HR, S, E1] =
    HPathEncoder[PathConverter[TD, TE, UT, S, E] :: H, TE :: HR, S, E1]({ h =>
      h1 =>
        val tp1 = h.head.encoder.encode(h1.head)
        val tp2 = hr.apply(h.tail).apply(h1.tail)
        tp1.append(tp2)
    })

}