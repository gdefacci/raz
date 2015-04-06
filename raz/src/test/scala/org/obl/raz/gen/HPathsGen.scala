package org.obl.raz.gen.exp

object HPathsGen {

  
  def gen(n:Int):String = {
    
    val dtyps = 1.to(n).map(i => s"+T$i").mkString(", ")
    
    val prevHPath = 
      if (n == 1) "HPathNil[_,_,_]"
      else {
        val ts = 1.to(n-1).map(i => s"T$i").mkString(", ")
        s"HPath${n-1}[_,_,_,$ts]"
      }
    
    s"  type HPath$n[+R <: RelativePathAspect,+A <: CanAddAspect,+P <: CanHavePrefixAspect,$dtyps] = HPathCons[$prevHPath,R,A,P,T$n]"
  }
  
  def main(args:Array[String]):Unit = {
    println( 1.to(22).map(gen).mkString("\n") )
    
    
  }
  
}