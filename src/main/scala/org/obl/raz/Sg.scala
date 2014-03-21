package org.obl.raz

sealed trait Sg

object PathSg {
  val empty = PathSg(Nil)
  
  def apply(path:String):PathSg = apply(Seq(path))
  def apply(path:Seq[String]):PathSg = new PathSg(path)
  
  def unapply(sg:PathSg):Option[Seq[String]] = Some(sg.path)
}

object QParamSg {
  
  def apply(name: String):QParamSg = apply(name, None)
  def apply(name: String, value: String):QParamSg = apply(name, Some(value))
  def apply(name: String, value: Option[String]):QParamSg = new QParamSg(name, value)
  
  def unapply(p:QParamSg) = Some(p.name -> p.value)
}

class PathSg private  (val path: Seq[String]) extends Sg {
  def isEmpty = path.isEmpty
  
  def add(str:String) = new PathSg(path ++ Seq(str))
  def add(othr:PathSg) = new PathSg(path ++ othr.path)
  
  override def hashCode = path.hashCode
  override def equals(a:Any) = a match {
    case p:PathSg if p.path == path => true
    case _ => false
  }
  override def toString = "PathSg("+path+")"
}
class QParamSg(val name: String, val value: Option[String]) extends Sg {
  
  override def hashCode = name.hashCode + value.hashCode * 7
  override def equals(a:Any) = a match {
    case p:QParamSg if p.name == name && p.value == value => true
    case _ => false
  }
  override def toString = "QParamSg("+name+", "+value.getOrElse("<empty>")+")"
}


/**
 * 
The alphanumeric characters "a" through "z", "A" through "Z" and "0" through "9" remain the same.
The unreserved characters ".", "-", "~", and "_" remain the same.
The general delimiters "@" and ":" remain the same.
The subdelimiters "!", "$", "&", "'", "(", ")", "*", "+", ",", ";", and "=" remain the same.
The space character " " is converted into %20.
All other characters are converted into one or more bytes using UTF-8 encoding and each byte is then represented by the 3-character string "%XY", where "XY" is the two-digit, uppercase, hexadecimal representation of the byte value.
*/
//private [raz] object UriPartEncode {
//  
//  private val hexDigit = Array[Char]('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');
//
//  private def escape(ch:Char):String = "%" + hexDigit(((ch >> 4) & 0xF)) + hexDigit(((ch >> 0) & 0xF))
//
//  val alphaNum = ('0' to '9').toSet ++ ('A' to 'Z') ++ ('a' to 'z')
//  var uriTemplateExtraUnescaped = Set('{', '}')
//  
//  val pathUnescaped:Set[Char] =  alphaNum ++ Set[Char]('.', '-', '~', '_', '@', ':' , '!', '$', '&', ''', '(', ')', '*', '+', ',', ';', '=')
//  val pathUriTemplateUnescaped:Set[Char] =  pathUnescaped ++ uriTemplateExtraUnescaped
//
//  val paramUnescaped = alphaNum ++ Set('.', '-', '*', '_') 
//  val paramUriTemplateUnescaped = paramUnescaped ++ uriTemplateExtraUnescaped
//
//  val fragmentUnescaped = alphaNum ++ Set('.', '-', '~', '_', '@', ':', '!', '$', '&', '(', ')', '*', '+', ',', ';', '=', '\'', '/', '?')
//
//  def encode(unaltered:Set[Char])(str:String):String = {
//    val sb = new StringBuilder()
//    str.foreach { ch =>
//      if (unaltered.contains(ch)) sb.append(ch)
//      else sb.append(escape(ch)) 
//    }
//    sb.toString
//  }
//
//}