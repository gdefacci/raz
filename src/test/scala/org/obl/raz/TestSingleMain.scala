package org.obl.raz

object TestSingleMain {
  
  def main(args:Array[String]):Unit = {
    
    import PathFs._
    
//    val u1 = Raz && paramValueVar[String]("p1") && paramValueVar[String]("p2") && paramValueVar[String]("p3")
    val u1 = Raz / "pippo" && paramValueVar[String]("p1") //&& paramValueVar[String]("p2") && paramValueVar[String]("p3")
    
    val u2 = Raz && ("a", "aaa")
    
    println(u1)
    println("u2 :" +u2)
    println( u1("a") )
    
  }

}