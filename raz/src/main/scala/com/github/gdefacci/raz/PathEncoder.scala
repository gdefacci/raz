package com.github.gdefacci.raz

import scala.language.higherKinds
import scala.language.implicitConversions
import shapeless._
import shapeless.ops.hlist.Tupler

trait PathEncoderMixin[T, S <: PathPosition, E <: PathPosition] extends Api.PathEncoder[T] {

  type EncoderType[T] <: PathEncoderMixin[T,S,E]
  
  protected def createEncoder[T1](f: T1 => TPath[S, E]): EncoderType[T1]

  def encode(t: T): TPath[S, E]

  def contramap[T1](f: T1 => T): EncoderType[T1] =
    createEncoder[T1](f.andThen(encode(_)))

}

sealed trait PathEncoder[T, S <: PathPosition, E <: PathPosition] extends PathEncoderMixin[T, S, E] {

  type EncoderType[T1] = PathEncoder[T1, S, E]
  protected def createEncoder[T1](f: T1 => TPath[S, E]) = PathEncoder(f)

  def append[S2 <: PathPosition, E2 <: PathPosition](suffix: TPath[S2, E2])(implicit suffixPathAppender: PathAppender[E, S2]): PathEncoder[T, S, E2] =
    RightPathEncoder(this, suffix)

  def prepend[S2 <: PathPosition, E2 <: PathPosition](prefix: TPath[S2, E2])(implicit prefixPathAppender: PathAppender[E2, S]): PathEncoder[T, S2, E] =
    LeftPathEncoder(prefix, this)

}

case class LeftPathEncoder[T, S0 <: PathPosition, E0 <: PathPosition, S <: PathPosition, E <: PathPosition] private[raz] (
    prefix: TPath[S0, E0], encoder: PathEncoder[T, S, E])(implicit prefixPathAppender:PathAppender[E0,S]) extends PathEncoder[T, S0, E] {
  
  def encode(t: T): TPath[S0, E] = {
    prefix.append(encoder.encode(t))
  }

  override def contramap[T1](f: T1 => T)  =
    new LeftPathEncoder[T1, S0, E0, S, E](prefix, encoder.contramap(f) )
  
}

final case class RightPathEncoder[T, S <: PathPosition, E <: PathPosition, S1 <: PathPosition, E1 <: PathPosition] private[raz] (
    encoder: PathEncoder[T, S, E], suffix: TPath[S1, E1])(implicit suffixPathAppender:PathAppender[E,S1]) extends PathEncoder[T, S, E1] {

  def encode(t: T): TPath[S, E1] = {
    encoder.encode(t).append(suffix)
  }

  override def contramap[T1](f: T1 => T)  =
    new RightPathEncoder[T1, S, E, S1, E1](encoder.contramap(f), suffix)

}

object PathEncoder {

  private[raz] def apply[T, S <: PathPosition, E <: PathPosition](f: T => TPath[S, E]): PathEncoder[T, S, E] = new PathEncoder[T, S, E] {
    def encode(t: T): TPath[S, E] = f(t)
  }

  implicit def toPathOps[T, S <: PathPosition, E <: PathPosition](p:PathEncoder[T,S,E]) = new PathOps(p)
  
  object Segment {

    private def create[T](f: T => String) = PathEncoder[T, PathPosition.Segment, PathPosition.Segment](t => Path.segments(f(t)) )

    val string = create[String](sg => sg)
    val int = create[Int](sg => sg.toString)
    val long = create[Long](sg => sg.toString)
    val boolean = create[Boolean](sg => sg.toString)

  }

  case class Param(name: String) {

    private def create[T](f: T => (String, Option[String])) = PathEncoder[T, PathPosition.Param, PathPosition.Param](t => Path.params(f(t)) )

    val string = create[String](par => name -> Some(par))
    val int = create[Int](par => name -> Some(par.toString))
    val long = create[Long](par => name -> Some(par.toString))
    val boolean = create[Boolean](par => name -> Some(par.toString))

  }

  object Param {

    private def create[T](f: T => (String, Option[String])) = PathEncoder[T, PathPosition.Param, PathPosition.Param](t => Path.params(f(t)))

    val string = create[(String, String)](par => par._1 -> Some(par._2))
    val int = create[(String, Int)](par => par._1 -> Some(par._2.toString))
    val long = create[(String, Long)](par => par._1 -> Some(par._2.toString))
    val boolean = create[(String, Boolean)](par => par._1 -> Some(par._2.toString))

  }

  object Fragment {
    private def create[T](f: T => String) = PathEncoder[T, PathPosition.Fragment, PathPosition.Fragment](t => Path.fragment(f(t)))

    val string = create[String](sg => sg)
    val int = create[Int](sg => sg.toString)
    val long = create[Long](sg => sg.toString)
    val boolean = create[Boolean](sg => sg.toString)

  }

}

