package org.obl.raz.gen

object PathFSumGen {

  
  def genPathFSum(n:Int) = {
	val tpars = 1.to(n).map(i => "T"+i).mkString(",")
    val typ = if (n == 1) tpars else "("+tpars+")"
    val n1 = n -1
    val vpars = 1.to(n1).map(i => s"v._$i" ).mkString(", ")
    
    val body = 
      if (n == 1) "h.value(v)"
      else s"sum(pathFSum$n1(h.head)($vpars), h.value(v._$n))"
    
	s"""
def pathFSum$n[$tpars](h: HResource$n[$tpars]): ($typ) => Path = { (v) =>
  $body
}"""
  }
  
  def main(args:Array[String]) = {
    println( 1.to(22).map(genPathFSum).mkString("\n") )
  }
  
}