package org.obl.raz

object TestSingleMain {

  import PathConverter._
  
  def main(args:Array[String]):Unit = {
    
//    val u1 = Raz && Param.string("p1") && Param.string("p2") && Param.string("p3")
    val u1 = Raz / "pippo" && Param.string("p1") //&& Param.string("p2") && Param.string("p3")
    
    val u2 = Raz && ("a", "aaa")
    
    println(u1)
    println("u2 :" +u2)
    println( u1("a") )
    
  }

}