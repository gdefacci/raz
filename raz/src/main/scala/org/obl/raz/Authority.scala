package org.obl.raz

import scala.language.implicitConversions

import scalaz.{-\/, \/, \/-}

sealed class Scheme(_render:String)  {
  lazy val render:String = _render.toLowerCase
  def apply(host:String, port:Int = 80) = new TPath[PathPosition.Absolute, PathPosition.Segment](Some(this), Some(Authority(host, port)), Nil, Nil, None)
  
  def apply(authority:Authority) = new TPath[PathPosition.Absolute, PathPosition.Segment](Some(this), Some(authority), Nil, Nil, None)
}

case object HTTP extends Scheme("http") 
case object HTTPS extends Scheme("https")
case object WS extends Scheme("ws")

object Scheme {
  
  val knowSchemes = Seq(HTTP, HTTPS, WS)
  
  def fromString(str:String) = 
    knowSchemes.find(_.render.equalsIgnoreCase(str)) match {
    case Some(v) => \/-(v)
    case None => -\/(new RuntimeException(s"unknown scheme $str"))
  }
}

case class Authority(host:String, port:Int) {
  
  lazy val render = {
    val prfx = s"//$host"
    if (port < 0 || port == 80) prfx
    else s"$prfx:$port"
  }
  
}