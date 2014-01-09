package org.obl.raz.gen

object HFGen {
  
  def genHF(n:Int) = {
    val tpars = 1.to(n).map(i => "T"+i).mkString(",")
    val vpars = 1.to(n).map(i => "v"+i).mkString(",")
    val typ = if (n == 1) tpars else "("+tpars+")"
    
    val body =
      if (n == 1) "(h => Base.pathFSum1(h))"
      else s"(h => ($vpars) => Base.pathFSum$n(h)(($vpars)))"
    
    s"implicit def hf$n[$tpars] = HF[HResource$n[$tpars], $typ => Path]$body"
  }
  
  def main(args:Array[String]):Unit = {
    println( 1.to(22).map(genHF).mkString("\n") )
  }
  
}