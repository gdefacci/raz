package org.obl.raz.gen

object PathMatcherGen {

  def genPathMatcher(n:Int):String = {
	val tpars = 1.to(n).map(i => "T"+i).mkString(",")
    val typ = if (n == 1) tpars else "("+tpars+")"


    
    s""" 
implicit def matcher$n[$tpars]: PathMatcher[HResource$n[$tpars], $typ] = {
  apply[HResource$n[$tpars], $typ](Base.matcher$n)
}"""
  }
  
  def main(args:Array[String]):Unit = {
    println( 1.to(22).map(genPathMatcher).mkString("\n") )
  }

}