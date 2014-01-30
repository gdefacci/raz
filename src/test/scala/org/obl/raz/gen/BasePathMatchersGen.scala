package org.obl.raz.gen.exp

object BasePathMatchersGen {

  def genMatcher(n:Int):String = {
 
    val tpars = 1.to(n).map(i => "T"+i).mkString(",")
    lazy val tpars1 = 1.to(n-1).map(i => "T"+i).mkString(",")
    val typ = if (n == 1) tpars else "("+tpars+")"
    lazy val rvaluePars = 
      if (n == 2) "r0.value"
      else 1.to(n-1).map( i => s"r0.value._$i").mkString(", ")
    
    val body =
	    if (n == 1) "PathHelper.subtract(pth, h.head.path).flatMap( r0 => h.value.matchPath(r0) )"
	    else 
s"""  matcher${n-1}(h.head)(pth).flatMap( r0 =>
    h.value.matchPath(r0.rest).map( r => 
      PathMatchResult(($rvaluePars, r.value), r.rest)))"""
      
    s"""
def matcher$n[$tpars](h: HPath$n[_,_,_,$tpars]): Path => Option[PathMatchResult[$typ, Path]] = { pth =>
$body    
}
"""
  }
  
  def main(args:Array[String]) = {
    println(1.to(22).map(genMatcher).mkString("\n"))
  }
}