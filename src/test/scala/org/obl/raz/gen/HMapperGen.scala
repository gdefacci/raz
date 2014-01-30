package org.obl.raz.gen.exp

object HMapperGen {

  
  def gen(n:Int):String = {
    val ts = 1.to(n).map(i => s"T$i" ).mkString(", ")
    val typ = 
      if (n == 1) "T1"
      else "("+ts+")"
    
    val n1 = n - 1
    val rootHeadExpr = s"h.${1.to(n).map(i => "head").mkString(".")}"
    val prev = 
      if (n==1) "Root"
      else {
        val tsN1 = 1.to(n-1).map(i => s"T$i" ).mkString(", ")
        s"HPath$n1[_,_,_,$tsN1]"
      }
    
  s"implicit def mapper$n[R <: RelativePathAspect, A <: CanAddAspect, P <: CanHavePrefixAspect, $ts] = new BaseMapper[$prev,R,A,P,T$n,$typ]"
  }
  
  def main(args:Array[String]):Unit = {
    
    println( 1.to(22).map(gen).mkString("\n") )
    
    
  }
  
  
}