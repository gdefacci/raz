package org.obl.raz.gen

object HParamsElemsGen {

  def hElem(n:Int):String = {
    
    val tpars = 1.to(n).map(i => "T"+i).mkString(",")
    val tparsCov = 1.to(n).map(i => "+T"+i).mkString(",")
    val tpars1 = 1.to(n-1).map(i => "T"+i).mkString(",")
	lazy val typeDelc = "type ParamsHResource"+n+"["+tparsCov+"] = "    
    
    if (n < 1) throw new Error("Invalid n:"+n)
    else if (n == 1) typeDelc+"ParamsHResource[HElemRoot, T1]"
    else typeDelc+"ParamsHResource[HResource"+(n-1)+"["+tpars1+"], T"+n+"]" 
  }
  
  
  def main(arags:Array[String]) = {
    println(1.to(22).map(hElem).mkString("\n"))
  }
  
}