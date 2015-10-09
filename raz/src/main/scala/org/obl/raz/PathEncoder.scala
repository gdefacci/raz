package org.obl.raz

import scalaz.{-\/, \/, \/-}
import exceptions._

import scala.language.implicitConversions
import scala.language.higherKinds 

trait PathEncoder[T] extends (T => Path){
  type Encoder[T] <: PathEncoder[T] 
  
  def encode(t:T):Path
  final def apply(t:T) = encode(t)
  
  protected def createEncoder[T1](f:T1 => Path):Encoder[T1]
  
  def contramap[T1](f:T1 => T):Encoder[T1] = {
    createEncoder[T1]((encode _) compose f)
  }
}

object PathEncoder {
  def apply[T](f:T => Path):PathEncoder[T] = {
    new PathEncoder[T] {
      type Encoder[T] = PathEncoder[T] 
      def encode(t:T) = f(t)
      protected def createEncoder[T1](f:T1 => Path) = PathEncoder.apply[T1](f)
    }
  }
  
  def prepend[T](sg:PathSg, e:PathEncoder[T]):PathEncoder[T] = {
    PathEncoder[T] { v =>
      val r1 = e.encode(v)
      Path(Path.baseOf(r1), sg.add(r1.path), r1.params, r1.fragment)
    }
  }
  
  def at[T](base:PathBase, e:PathEncoder[T]):PathEncoder[T] = {
    PathEncoder[T] { v =>
      val r1 = e.encode(v)
      Path(Some(base), r1.path, r1.params, r1.fragment)
    }
  }
  
  def fromPath(p:Path):PathEncoder[Path] = apply[Path](p => p)
  
  def seq[T](enc:PathEncoder[T]):PathEncoder[Seq[T]] = {
    PathEncoder[Seq[T]]( sq => PathUtils.mergeAll(sq.map(enc.encode(_))) )
  }
  
  def opt[T](enc:PathEncoder[T]):PathEncoder[Option[T]] = {
    PathEncoder[Option[T]]( sq => PathUtils.mergeAll(sq.map(enc.encode(_)).toSeq) )
  }
  
  private[raz] def withSuffix[T](e:PathEncoder[T], sfx:Path) = {
    PathEncoder[T]( v => PathUtils.merge( e.encode(v) ,sfx ))
  }
  
  lazy val stringSegment = PathEncoder[String](sg => RelativePath(PathSg(sg)))
  
  def toStringSegment[T] = stringSegment.contramap { i:T => i.toString }
  
  lazy val intSegment = toStringSegment[Int]
  lazy val longSegment = toStringSegment[Long]
  lazy val booleanSegment = toStringSegment[Boolean]
  
  def enumSegment[E <: Enumeration] = toStringSegment[E#Value]
  def enumValueSegment[V <: Enumeration#Value] = toStringSegment[V]
  
  lazy val optStringParam = PathEncoder[(String,Option[String])](sg => RelativePath(QParamSg(sg._1, sg._2) :: Nil))
  lazy val stringParam = optStringParam.contramap { p:(String,String) => (p._1 -> Some(p._2)) }
  
  def toStringParam[I] = stringParam.contramap { i:(String, I) => i._1 -> i._2.toString }
  
  lazy val intParam = toStringParam[Int]
  lazy val longParam = toStringParam[Long]
  lazy val booleanParam = toStringParam[Boolean]
  
  def enumParam[E <: Enumeration] = toStringParam[E#Value]
  def enumValueParam[V <: Enumeration#Value] = toStringParam[V]

  def optStringParamValue(name:String) = PathEncoder[(Option[String])](sg => RelativePath(QParamSg(name, sg) :: Nil))
  def stringParamValue(name:String) = optStringParamValue(name).contramap { p:String => Some(p) }

  def toStringParam[I](name:String) = stringParamValue(name:String).contramap { i:I => i.toString }
  
  def intParamValue(name:String) = toStringParam[Int](name)
  def longParamValue(name:String) = toStringParam[Long](name)
  def booleanParamValue(name:String) = toStringParam[Boolean](name)
  
  def enumParamValue[E <: Enumeration](nm:String) = toStringParam[E#Value](nm)
  def enumValueParamValue[V <: Enumeration#Value](nm:String) = toStringParam[V](nm:String)

  lazy val stringFragment = PathEncoder[String](sg => RelativePath(fragment = sg))
  
 implicit def apply[H <: HPath, P <: PathPosition, S <: P, E](h: H)(implicit hf: EncHPathF[H, E, BasePath[P, S]]): PathEncoder[E] = {
    val e = hf.apply(h)
    PathEncoder(e)
  }

  
}
