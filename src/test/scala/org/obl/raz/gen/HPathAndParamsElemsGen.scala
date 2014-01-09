package org.obl.raz.gen

object HPathAndParamsElemsGen {

  def hElem(n:Int):String = {
    
    val tpars = 1.to(n).map(i => "T"+i).mkString(",")
    val tparsCov = 1.to(n).map(i => "+T"+i).mkString(",")
    val tpars1 = 1.to(n-1).map(i => "T"+i).mkString(",")
	lazy val typeDelc = "type PathAndParamsHResource"+n+"["+tparsCov+"] = "    
    
    if (n < 1) throw new Error("Invalid n:"+n)
    else if (n == 1) typeDelc+"PathAndParamsHResource[RootPath, T1]"
    else typeDelc+"PathAndParamsHResource[PathHResource"+(n-1)+"["+tpars1+"], T"+n+"]" 
  }
  
  
  def main(arags:Array[String]) = {
    println(1.to(22).map(hElem).mkString("\n"))
  }
  
}