package org.obl.raz

trait Converter[A, B] {

  def apply(a: A): B
  def unapply(b: B): Option[A]

}

object Converter {
  def apply[A, B](f: A => B, rev: B => Option[A]) =
    new Converter[A, B] {
      def apply(a: A): B = f(a)
      def unapply(b: B): Option[A] = rev(b)
    }
}

class StringConverter[A](f: A => Option[String], rev: Option[String] => Option[A]) extends Converter[A, Option[String]] {
  def apply(a: A) = f(a)
  def unapply(b: Option[String]) = rev(b)
}

object StringConverter {
  def apply[A](f: A => Option[String], rev: Option[String] => Option[A]) = new StringConverter[A](f, rev)

  private def trap[A](a: => A): Option[A] = {
    try {
      Some(a)
    } catch {
      case e: Exception => None
    }
  }

//  implicit def opt[T](implicit sc:StringConverter[T]):StringConverter[Option[T]] =
//    apply[Option[T]](t => t.flatMap(sc.apply), (optstr => Some(sc.unapply(optstr)))) 
  
  implicit val int = StringConverter[Int](i => Some(i.toString), s => s.flatMap(s => trap(s.toInt)))
  implicit val long = StringConverter[Long](i => Some(i.toString), s => s.flatMap(s => trap(s.toLong)))
  implicit val boolean = StringConverter[Boolean](i => Some(i.toString), s => s.flatMap(s => trap(java.lang.Boolean.valueOf(s)) ))
  implicit val string = StringConverter[String](i => Some(i), s => s)

  def enum[T <: Enumeration](e:T) = enumValue[T, T#Value](e)
//    StringConverter[T#Value](e => Some(e.toString), { optStr =>
//    optStr match {
//      case None => None
//      case Some(nm) => {
//        trap(e.withName(nm))
//      }
//    }
//  })
  
  def enumValue[T <: Enumeration, V1 <: T#Value](e:T) = StringConverter[V1](e => Some(e.toString), { optStr =>
    optStr match {
      case None => None
      case Some(nm) => {
        trap(e.withName(nm).asInstanceOf[V1])
      }
    }
  })
  
}