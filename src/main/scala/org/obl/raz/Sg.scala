package org.obl.raz

sealed trait Sg

object PathSg {
  val empty = PathSg(Nil)
  
  def apply(path:Seq[String]) = new PathSg(path)
  def apply(path:String):PathSg = apply(Seq(path))
  
  def unapply(sg:PathSg):Option[Seq[String]] = Some(sg.path)
}

object QParamSg {
  val defaultAttrRenderer:(Boolean, (String,Option[String])) => String = { (isFirst, nameValue) =>
    val (name, value) = nameValue
    val res = value match {
      case None => name
      case Some(v) =>  name + "=" + v
    } 
    if (isFirst) "?" + res
    else "&" + res
  }
  
  def apply(name: String):QParamSg = apply(name, None)
  def apply(name: String, value: String):QParamSg = apply(name, Some(value))
  def apply(name: String, value: Option[String]):QParamSg = apply(name, value, defaultAttrRenderer)
  def apply(name: String, value: Option[String], renderer:(Boolean, (String,Option[String])) => String):QParamSg = new QParamSg(name, value, renderer)
  
  def unapply(p:QParamSg) = Some(p.name -> p.value)
}

class PathSg(val path: Seq[String]) extends Sg {
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
class QParamSg(val name: String, val value: Option[String], private[raz] renderer:(Boolean, (String,Option[String])) => String) extends Sg {
  def render(isFirst:Boolean) = renderer(isFirst, name -> value)
  
  override def hashCode = name.hashCode + value.hashCode * 7
  override def equals(a:Any) = a match {
    case p:QParamSg if p.name == name && p.value == value => true
    case _ => false
  }
  override def toString = "QParamSg("+name+", "+value.getOrElse("<empty>")+")"
}