package org.obl.raz

import scalaz.{ -\/, \/, \/- }
import shapeless._
import scala.language.implicitConversions
import scala.language.higherKinds
import shapeless.ops.hlist.Tupler

case class MatchResult[+T](value: T, rest: Path)

trait PathDecoderMixin[+T] {

  type DecoderType[T] <: PathDecoderMixin[T]

  protected def createDecoder[T1](f: Path => Throwable \/ MatchResult[T1]): DecoderType[T1]

  def decode(path: Path): Throwable \/ MatchResult[T]

  def decodeFull(path: Path): Throwable \/ T = {
    decode(path) match {
      case -\/(err) => -\/(err)
      case \/-(pmr) if PathDecoder.isEmpty(pmr.rest) => \/-(pmr.value)
      case \/-(pmr) => -\/(new PathExpectationException(Path, pmr.rest))
    }
  }

  def map[T1](f: T => T1): DecoderType[T1] = createDecoder[T1] { i: Path =>
    decode(i).flatMap { mr => \/.fromTryCatchNonFatal(MatchResult[T1](f(mr.value), mr.rest)) }
  }
  
  def unapply[I](i: I)(implicit pathExtractor: PathExtractor[I]): Option[T] = {
    pathExtractor.apply(i).flatMap(p => decodeFull(p)).toOption
  }

}

trait PathDecoder[T, S <: PathPosition, E <: PathPosition] extends PathDecoderMixin[T] with Api.PathDecoder[T] {

  val pathDecoder = this

  type DecoderType[T1] = PathDecoder[T1, S, E]
  protected def createDecoder[T1](f: Path => Throwable \/ MatchResult[T1]) = PathDecoder[T1, S, E](f)

  def andThen[T1, S1 <: PathPosition, E1 <: PathPosition](d:PathDecoder[T1,S1,E1])(implicit pa:PathAppender[E,S1]) =
    PathDecoder[T1, S, E1] { p =>
      pathDecoder.decode(p).flatMap {
        case MatchResult(v, rest)  => d.decode(rest)
      }
    }
  
  def orElse[ST >: T, T1 <: ST](d: => PathDecoder[T1, S, E]): PathDecoder[ST, S, E] = PathDecoder[ST, S, E] { pth =>
    decode(pth) match {
      case \/-(r) => \/-(r)
      case _ => d.decode(pth)
    }
  }

  private[raz] def decoderAt(path: Path): DecoderType[T] with Api.PartialPathDecoder[T] = PartialPathDecoder(this, path)
}

final case class LeftPathDecoder[T, S0 <: PathPosition, E0 <: PathPosition, S <: PathPosition, E <: PathPosition](
    prefix: TPath[S0, E0], decoder: PathDecoder[T, S, E]) extends PathDecoder[T, S0, E] {

  def decode(path: Path): Throwable \/ MatchResult[T] = {
    for {
      p1 <- DecodeUtils.subtract(path, prefix)
      p2 <- decoder.decode(p1)
    } yield MatchResult(p2.value, p2.rest)
  }

  override def map[T1](f: T => T1): PathDecoder[T1, S0, E] =
    new LeftPathDecoder(prefix, decoder.map(f))

}

final case class RightPathDecoder[T, S <: PathPosition, E <: PathPosition, S1 <: PathPosition, E1 <: PathPosition](
    decoder: PathDecoder[T, S, E], suffix: TPath[S1, E1]) extends PathDecoder[T, S, E1] {

  def decode(path: Path): Throwable \/ MatchResult[T] = {
    for {
      p2 <- decoder.decode(path)
      p3 <- DecodeUtils.subtract(p2.rest, suffix)
    } yield MatchResult(p2.value, p3)
  }

  override def map[T1](f: T => T1): PathDecoder[T1, S, E1] =
    new RightPathDecoder(decoder.map(f), suffix)

}


object PathDecoder {
  
  implicit def toPathOps[T, S <: PathPosition, E <: PathPosition](p:PathDecoder[T,S,E]) = new PathOps(p)

  private[raz] def isEmpty(p: Path) = p.segments.isEmpty && p.params.isEmpty && p.fragment.isEmpty

  private[raz] def apply[T, S <: PathPosition, E <: PathPosition](f: Path => Throwable \/ MatchResult[T]) =
    new PathDecoder[T, S, E] {
      def decode(p: Path): Throwable \/ MatchResult[T] =
        \/.fromTryCatchNonFatal(f(p)).flatMap(i => i)
    }

  def opt[T, S <: PathPosition, E <: PathPosition](dec: PathDecoder[T, S, E]): PathDecoder[Option[T], S, E] = {
    PathDecoder[Option[T], S, E] { pth: Path =>
      val ri = dec.decode(pth)
      ri match {
        case -\/(_) => \/-(MatchResult(None, pth))
        case \/-(v) => \/-(MatchResult(Some(v.value), v.rest))
      }
    }
  }
  
  def schemAndAuthority(s:Option[Scheme], a:Option[Authority]) = PathDecoder[(Option[Scheme], Option[Authority]), PathPosition.Absolute, PathPosition.Segment] { p =>
    if (p.scheme == s && p.authority == a) \/-(MatchResult(p.scheme -> p.authority, p.copy(scheme=None, authority = None))) 
    else -\/(PathExpectationException(Path.copy(scheme=s, authority = a), p))
  }
    
  def segments(sgs:Seq[String]) = PathDecoder[Unit, PathPosition.Segment, PathPosition.Segment] { p:Path =>
    if (p.authority.isEmpty && p.segments.startsWith(sgs)) \/-(MatchResult((), p.copy(segments = p.segments.drop(sgs.length))))
    else -\/(PathExpectationException(Path.copy(segments = sgs), p))
  }
    
  def params(pars: Seq[(String, Option[String])]) = PathDecoder[Seq[(String, Option[String])], PathPosition.Param, PathPosition.Param] { p: Path =>
      if (!p.authority.isEmpty || !p.segments.isEmpty) -\/(new RuntimeException(s"expecting parameters got $p"))
      
      val matchParsIt = pars.iterator
      var ok = true
      var remPars = p.params
      while (matchParsIt.hasNext && ok) {
        val par = matchParsIt.next
        val z: (Seq[(String, Option[String])], Option[(String, Option[String])]) = Nil -> None 
        val (rpars, res) = remPars.foldLeft(z) { (acc1, rpar) =>
          val (rpars, res) = acc1
          res match {
            case None if (rpar == par) => rpars -> Some(rpar)
            case r => (rpars :+ rpar) -> r
          }
        } 
        if (res.isEmpty) ok = false
        else remPars = rpars
      }
      if (!ok) -\/(PathExpectationException(Path.copy(params = pars), p))
      else \/-(MatchResult(pars, p.copy(params = remPars)))
    }
  
  def fragment(frg:Option[String]) = PathDecoder[Option[String], PathPosition.Fragment, PathPosition.Fragment] { p: Path =>
    if (frg.isEmpty) \/-(MatchResult(None, p)) 
    else if (p.fragment == frg) \/-(MatchResult(p.fragment, p.copy(fragment = None))) 
    else -\/(PathExpectationException(Path.copy(fragment = frg), p))
 }
  
  def path(p:Path):PathDecoder[Path, PathPosition.Absolute, PathPosition.Fragment] =  
    schemAndAuthority(p.scheme, p.authority).andThen(
        segments(p.segments)).andThen(
            params(p.params)).andThen(
                fragment(p.fragment)).map( x => p )
  

  object Segment {

    val string = PathDecoder[String, PathPosition.Segment, PathPosition.Segment] { p: Path =>
      p.segments.headOption match {
        case None => -\/(NoMoreSegments)
        case Some(hd) => \/-(MatchResult(DecodeUtils.replacePercentTriplets(hd), p.copy(segments = p.segments.tail)))
      }
    }

    val int = string.map(_.toInt)
    val long = string.map(_.toLong)
    val boolean = string.map(_.toBoolean)

    def enum[E <: Enumeration](e: E) = string.map(e.withName(_))

  }

  object Param {

    def apply(pred: (String, Option[String]) => Boolean) = PathDecoder[Option[String], PathPosition.Param, PathPosition.Param] { p: Path =>
      val z: (Option[Option[String]], Seq[(String, Option[String])]) = None -> Nil
      p.params.foldLeft(z) { (acc, par) =>
        val (res, pars) = acc
        res match {
          case r @ Some(_) => r -> (pars :+ par)
          case _ =>
            if (pred(par._1, par._2)) (Some(par._2) -> pars) else (None -> (pars :+ par))
        }
      } match {
        case (Some(r), pars) => \/-(MatchResult(r, p.copy(params = pars)))
        case _ => \/-(MatchResult(None, p))
      }
    }

  }

  case class Param(name: String) {

    val optString = Param((parName, value) => parName == name)

    val string: PathDecoder[String, PathPosition.Param, PathPosition.Param] = optString.map(_.getOrElse(throw new MissingParameter(name)))

    val int = string.map(_.toInt)
    val long = string.map(_.toLong)
    val boolean = string.map(_.toBoolean)

    def enum[E <: Enumeration](e: E) = string.map(e.withName(_))

  }

  object Fragment {

    val string = PathDecoder[String, PathPosition.Fragment, PathPosition.Fragment] { p: Path =>
      p.fragment match {
        case None => -\/(NoFragment)
        case Some(hd) => \/-(MatchResult(hd, p.copy(fragment = None)))
      }
    }

    val int = string.map(_.toInt)
    val long = string.map(_.toLong)
    val boolean = string.map(_.toBoolean)

    def enum[E <: Enumeration](e: E) = string.map(e.withName(_))

  }

  implicit def hTupleDecoder[H <: HList, TUP, S <: PathPosition, E <: PathPosition](h: H)(implicit tpd:ToPathDecoder[H,TUP,S,E]): PathDecoder[TUP, S, E] =
    tpd(h)
}

trait PathMatchDecoder extends PathDecoder[Path, PathPosition, PathPosition] with Api.PathMatchDecoder 

object PathMatchDecoder {
  def apply(pth:Path):PathMatchDecoder = apply(pth, pth)
  
  def apply(pth:Path, decodePath:Path) = new PathMatchDecoder {
    lazy val path = pth
    def decode(p: Path) = {
      if (p == decodePath) \/-(MatchResult(decodePath, Path.empty))
      else -\/(PathExpectationException(decodePath, p))
    }
  }
}

final case class PartialPathDecoder[T, S <: PathPosition, E <: PathPosition](fullPath: PathDecoder[T, S, E], prefix: Path) extends PathDecoder[T, S, E] with Api.PartialPathDecoder[T] {

  def decode(p: Path) = fullPath.decode(prefix.merge(p))

}