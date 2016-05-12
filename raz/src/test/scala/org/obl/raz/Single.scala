package org.obl.raz

import scalaz.\/-
import sample._

object Single extends App {

   
  //val p = Path(None,None,List("U3*_", "@/h&H06R", "K)25~", "28B", "$)H", "~4qWg4s]"),List(("A7*5h,L6",Some("7")), ("37",None)),Some("D"))
  
  val p = Path(None,None,List("/"),Nil,None)
  
  println(p.render)
  val r1 = Path.fromJavaUri(new java.net.URI(p.render))
  println(r1)
  println(\/-(p))
  assert( r1.map(_.scheme) == \/-(p.scheme), "scheme") 
  assert( r1.map(_.authority) == \/-(p.authority), "authority")
  println(r1.map(_.segments))
  println(r1.map(_.segments.length) )
  assert( r1.map(_.segments) == \/-(p.segments), "segments") 
  assert( r1.map(_.params) == \/-(p.params), "params") 
  assert( r1 == \/-(p), "full") 
  
  
}