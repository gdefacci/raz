package org.obl.raz.gen.exp

object PathMatcherGen {

  def genPathMatcher(n:Int):String = {
	val tpars = 1.to(n).map(i => "T"+i).mkString(",")
    val typ = if (n == 1) tpars else "("+tpars+")"


    
    s""" 
implicit def matcher$n[$tpars]: PathMatcher[HPath$n[_,_,_,$tpars], $typ] = {
  apply[HPath$n[_,_,_,$tpars], $typ](Matchers.matcher$n)
}"""
  }
  
  def main(args:Array[String]):Unit = {
    println( 1.to(22).map(genPathMatcher).mkString("\n") )
  }

}