package org.obl.raz

import shapeless._
import scalaz.{ -\/, \/, \/- }

trait HPathDecoder[H <: HList, HR <: HList, S <: PathPosition, E <: PathPosition] {
  def apply(h: H): Path => Throwable \/ MatchResult[HR]
}

object HPathDecoder {

  private def apply[H <: HList, HR <: HList, S <: PathPosition, E <: PathPosition](f: H => Path => Throwable \/ MatchResult[HR]) = new HPathDecoder[H, HR, S, E] {
    def apply(h: H): Path => Throwable \/ MatchResult[HR] = f(h)
  }

  implicit def hnilHPathDecoder[T, S <: PathPosition, E <: PathPosition]: HPathDecoder[PathDecoder[T, S, E] :: HNil, T :: HNil, S, E] =
    HPathDecoder[PathDecoder[T, S, E] :: HNil, T :: HNil, S, E] { h =>
      h1 =>
        h.head.decode(h1).map( res => MatchResult(res.value :: HNil, res.rest) )
    }

  implicit def hconsHPathDecoder[H <: HList, HR <: HList, T, S <: PathPosition, E <: PathPosition, S1 <: PathPosition, E1 <: PathPosition](
      implicit hr: HPathDecoder[H, HR, S1, E1],
      pathAppender: PathAppender[E, S1]): HPathDecoder[PathDecoder[T, S, E] :: H, T :: HR, S, E1] =
    HPathDecoder[PathDecoder[T, S, E] :: H, T :: HR, S, E1] { h =>
      pth =>
        h.head.decode(pth).flatMap { mr =>
          hr.apply(h.tail).apply(mr.rest).map { mr1 =>
            MatchResult(mr.value :: mr1.value, mr1.rest)
          }
        }
    }

  implicit def hnilCodecHPathDecoder[TD, TE, S <: PathPosition, E <: PathPosition]: HPathDecoder[PathCodec[TD, TE, S, E] :: HNil, TD :: HNil, S, E] =
    HPathDecoder[PathCodec[TD, TE, S, E] :: HNil, TD :: HNil, S, E] { h =>
      h1 =>
        h.head.decoder.decode(h1).map( res => MatchResult(res.value :: HNil, res.rest) )
    }

  implicit def hconsCodecHPathDecoder[H <: HList, HR <: HList, TD, TE, S <: PathPosition, E <: PathPosition, S1 <: PathPosition, E1 <: PathPosition](implicit hr: HPathDecoder[H, HR, S1, E1], pathAppender: PathAppender[E, S1]): HPathDecoder[PathCodec[TD, TE, S, E] :: H, TD :: HR, S, E1] =
    HPathDecoder[PathCodec[TD, TE, S, E] :: H, TD :: HR, S, E1] { h =>
      pth =>
        h.head.decoder.decode(pth).flatMap { mr =>
          hr.apply(h.tail).apply(mr.rest).map { mr1 =>
            MatchResult(mr.value :: mr1.value, mr1.rest)
          }
        }
    }

  implicit def hnilConverterHPathDecoder[TD, TE, UT, S <: PathPosition, E <: PathPosition]: HPathDecoder[PathConverter[TD, TE, UT, S, E] :: HNil, TD :: HNil, S, E] =
    HPathDecoder[PathConverter[TD, TE, UT, S, E] :: HNil, TD :: HNil, S, E] { h =>
      h1 =>
        h.head.decoder.decode(h1).map( res => MatchResult(res.value :: HNil, res.rest) )
    }

  implicit def hconsConverterHPathDecoder[H <: HList, HR <: HList, TD, TE, UT, S <: PathPosition, E <: PathPosition, S1 <: PathPosition, E1 <: PathPosition](implicit hr: HPathDecoder[H, HR, S1, E1], pathAppender: PathAppender[E, S1]): HPathDecoder[PathConverter[TD, TE, UT, S, E] :: H, TD :: HR, S, E1] =
    HPathDecoder[PathConverter[TD, TE, UT, S, E] :: H, TD :: HR, S, E1] { h =>
      pth =>
        h.head.decoder.decode(pth).flatMap { mr =>
          hr.apply(h.tail).apply(mr.rest).map { mr1 =>
            MatchResult(mr.value :: mr1.value, mr1.rest)
          }
        }
    }

}
