package org.obl.raz

import scala.language.implicitConversions
import scalaz.{-\/, \/, \/-}

import scala.language.higherKinds

abstract class PathCodec[D, E](d: Path => Throwable \/ PathMatchResult[D, Path], e: E => Path) extends PathDecoder[D] with PathEncoder[E] {
  def decode(path: Path) = d(path)
  def encode(t: E): Path = e(t)

  type Codec[D1,E1] <: PathCodec[D1, E1]
  protected def codecFactory[D1,E1](d: Path => Throwable \/ PathMatchResult[D1, Path], e: E1 => Path):Codec[D1,E1]
  
  type Decoder[T] = Codec[T, E]
  type Encoder[T] = Codec[D, T]
  
  protected def createDecoder[T1](f: Path => Throwable \/ PathMatchResult[T1, Path]) = codecFactory[T1, E](f, e)
  protected def createEncoder[T1](f: T1 => Path) = codecFactory[D, T1](d, f)

  def caseMap[T1](tupled: D => T1, caseUnapply: T1 => Option[E]): Codec[T1, T1] = {
    codecFactory(d.andThen(_.flatMap( p => \/.fromTryCatchNonFatal(p.mapValue(tupled) ))), e.compose((t1: T1) => caseUnapply(t1).get))
  }

}

object PathCodec {

  type Symmetric[T] = PathCodec[T,T]
  
  private def apply[D, E](d: Path => Throwable \/ PathMatchResult[D, Path], e: E => Path):PathCodec[D,E] = {
    new PathCodec[D,E](d,e) {
      type Codec[D1,E1] = PathCodec[D1, E1]
      protected def codecFactory[D1,E1](d: Path => Throwable \/ PathMatchResult[D1, Path], e: E1 => Path):Codec[D1,E1] = PathCodec[D1,E1](d,e)
    }
  }
  
  implicit def apply[P <: Path, H <: HPath, D, E](h: H)(implicit pathMatcher: PathMatcher[H, D], hf: EncHPathF[H, E, P]): PathCodec[D, E] = {
    val d = pathMatcher.decoder(h)
    val e = hf.apply(h)
    PathCodec[D,E](d.decode(_), PathEncoder(e))
  }
  
  def prependEncoder[D, E](sg:PathSg, pc:PathCodec[D,E]) = {
    PathCodec[D,E](
      pc.decode(_),
      PathEncoder.prepend(sg, PathEncoder(pc.encode _)).encode(_)
    )
  }
  
  def encoderAt[D, E](host:PathBase, pc:PathCodec[D,E]) = {
    PathCodec[D,E](
      pc.decode(_),
      PathEncoder.at(host, PathEncoder(pc.encode _))
    )
  }
  
}