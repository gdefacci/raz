package org.obl.raz

trait Protocol {
  def render:String
  
  def apply(host:String, port:Int = 80) = PathBase(this, host, port)
}
case object HTTP extends Protocol {
  val render = "http"
}
case object HTTPS extends Protocol {
  val render = "https"
}

case object WS extends Protocol {
  val render = "ws"
}

final case class PathBase(protocol:Protocol, host:String, port:Int) extends BasePathSegmentAdder[BasePosition] with BaseParamAdder[BasePosition, SegmentPosition] {
  lazy val segmentAdderSelf = AbsolutePath(this)
	lazy val paramAdderSelf = segmentAdderSelf 
  
  def render = {
    val prfx = s"${protocol.render}://$host"
    if (port < 0 || port == 80) prfx
    else s"$prfx:$port"
  }
}