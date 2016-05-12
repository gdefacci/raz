package org.obl.raz

import scala.language.higherKinds
import scala.language.implicitConversions
import shapeless._
import shapeless.ops.hlist.Tupler

trait PathEncoderMixin[T, S <: PathPosition, E <: PathPosition] extends Api.PathEncoder[T] {

  type EncoderType[T] <: PathEncoderMixin[T,S,E]
  
  protected def createEncoder[T1](kind: UriTemplate.Kind, f: T1 => TPath[S, E]): EncoderType[T1]

  def encode(t: T): TPath[S, E]

  private[raz] def kind: UriTemplate.Kind

  def contramap[T1](f: T1 => T): EncoderType[T1] =
    createEncoder[T1](kind, f.andThen(encode(_)))

}

sealed trait PathEncoder[T, S <: PathPosition, E <: PathPosition] extends PathEncoderMixin[T, S, E] {

  type EncoderType[T1] = PathEncoder[T1, S, E]
  protected def createEncoder[T1](kind: UriTemplate.Kind, f: T1 => TPath[S, E]) = PathEncoder(kind, f)

  def append[S1 <: PathPosition, E1 <: PathPosition](suffix: TPath[S1, E1])(implicit pathAppender: PathAppender[E, S1]): PathEncoder[T, S, E1] =
    AroundPathEncoder(Path.empty, this, suffix)

  def prepend[S1 <: PathPosition, E1 <: PathPosition](prefix: TPath[S1, E1])(implicit pathAppender: PathAppender[E1, S]): PathEncoder[T, S1, E] =
    AroundPathEncoder(prefix, this, Path.empty)

}

final case class AroundPathEncoder[T, S0 <: PathPosition, E0 <: PathPosition, S <: PathPosition, E <: PathPosition, S1 <: PathPosition, E1 <: PathPosition] private[raz] (
    prefix: TPath[S0, E0], encoder: PathEncoder[T, S, E], suffix: TPath[S1, E1]) extends PathEncoder[T, S0, E1] {

  lazy val kind = encoder.kind

  def encode(t: T): TPath[S0, E1] = {
    prefix.append(encoder.encode(t).append(suffix))
  }

  override def contramap[T1](f: T1 => T): AroundPathEncoder[T1, S0, E0, S, E, S1, E1] =
    AroundPathEncoder[T1, S0, E0, S, E, S1, E1](prefix, encoder.contramap[T1](f), suffix)

  override def append[S2 <: PathPosition, E2 <: PathPosition](suffix: TPath[S2, E2])(implicit pathAppender: PathAppender[E1, S2]): PathEncoder[T, S0, E2] =
    AroundPathEncoder(prefix, encoder, this.suffix.append(suffix))

  override def prepend[S2 <: PathPosition, E2 <: PathPosition](prefix: TPath[S2, E2])(implicit pathAppender: PathAppender[E2, S0]): PathEncoder[T, S2, E1] =
    AroundPathEncoder(prefix.append(this.prefix), encoder, suffix)

}

trait ToPathEncoder[-H <: HList, T, S <: PathPosition, E <: PathPosition] {
  def apply(h: H): PathEncoder[T, S, E]
}

object ToPathEncoder {

  implicit def htuple[H <: HList, HR <: HList, TUP, S <: PathPosition, E <: PathPosition](
      implicit hr: HPathEncoder[H, HR, S, E], 
      tupler: Tupler.Aux[HR, TUP], 
      gen: Generic.Aux[TUP, HR]) =
    new ToPathEncoder[H, TUP, S, E] {
      def apply(h: H): PathEncoder[TUP, S, E] = {
        val hrEnc = hr.apply(h)
        val kind = hr.kind(h)
        PathEncoder(kind, { (tup: TUP) =>
          val h1 = gen.to(tup)
          hrEnc.apply(h1)
        })
      }
    }

  implicit def htuple1[T, S <: PathPosition, E <: PathPosition] =
    new ToPathEncoder[PathEncoder[T,S,E] :: HNil, T, S, E] {
      def apply(h: PathEncoder[T,S,E] :: HNil): PathEncoder[T, S, E] = {
        h.head
      }
    }

}

object PathEncoder {

  private[raz] def apply[T, S <: PathPosition, E <: PathPosition](k: UriTemplate.Kind, f: T => TPath[S, E]): PathEncoder[T, S, E] = new PathEncoder[T, S, E] {
    def encode(t: T): TPath[S, E] = f(t)
    lazy val kind = k
  }

  implicit def toPathEncoderBuilder[T, S <: PathPosition, E <: PathPosition](pe: PathEncoder[T, S, E]): PathEncoderBuilder[PathEncoder[T, S, E] :: HNil, HNil, S, T, S, E] =
    PathEncoderBuilder[PathEncoder[T, S, E] :: HNil, HNil, S, T, S, E](pe :: HNil, HNil, pe)

  implicit def hTupleEncoder[H <: HList, TUP, S <: PathPosition, E <: PathPosition](h: H)(implicit tpe:ToPathEncoder[H,TUP,S,E]) = {
    tpe(h)
  }  
    
  object Segment {

    private def create[T](f: T => String) = PathEncoder[T, PathPosition.Segment, PathPosition.Segment](UriTemplate.Segment, t => Path / f(t))

    val string = create[String](sg => sg)
    val int = create[Int](sg => sg.toString)
    val long = create[Long](sg => sg.toString)
    val boolean = create[Boolean](sg => sg.toString)

  }

  case class Param(name: String) {

    private def create[T](f: T => (String, Option[String])) = PathEncoder[T, PathPosition.Param, PathPosition.Param](UriTemplate.ParamWithName(name), { t =>
      f(t) match {
        case (nm, None) => Path && nm
        case (nm, Some(v)) => Path && (nm, v)
      }
    })

    val string = create[String](par => name -> Some(par))
    val int = create[Int](par => name -> Some(par.toString))
    val long = create[Long](par => name -> Some(par.toString))
    val boolean = create[Boolean](par => name -> Some(par.toString))

  }

  object Param {

    private def create[T](f: T => (String, Option[String])) = PathEncoder[T, PathPosition.Param, PathPosition.Param](UriTemplate.Param, { t =>
      f(t) match {
        case (nm, None) => Path && nm
        case (nm, Some(v)) => Path && (nm, v)
      }
    })

    val string = create[(String, String)](par => par._1 -> Some(par._2))
    val int = create[(String, Int)](par => par._1 -> Some(par._2.toString))
    val long = create[(String, Long)](par => par._1 -> Some(par._2.toString))
    val boolean = create[(String, Boolean)](par => par._1 -> Some(par._2.toString))

  }

  object Fragment {
    private def create[T](f: T => String) = PathEncoder[T, PathPosition.Fragment, PathPosition.Fragment](UriTemplate.Fragment, t => Path &# f(t))

    val string = create[String](sg => sg)
    val int = create[Int](sg => sg.toString)
    val long = create[Long](sg => sg.toString)
    val boolean = create[Boolean](sg => sg.toString)

  }

}

