package org.obl.raz

object Sample {

  def main(args:Array[String]):Unit =  {
//    matcherSample
//    matcherSample1
//    appendSample
//    utTest
//    parSeqTest
    optTest
    noneTest 
//    angularUtTest
  }
  
  import PathFs._

  def rootSample = {
    val u2 = Raz.path("a").path(pathVar[Int]).path(pathVar[Int])
    
    Raz.path("a").root
    Raz.path("a").path(pathVar[Int]).path(pathVar[Int]).root
    Raz.path("a").path(pathVar[Int]).path(pathVar[Int]).param("aa", "aa").root
    Raz.path("a").path(pathVar[Int]).path(pathVar[Int]).param("aa", "aa").param( paramValueVar[String]("aaa") ).root

  }
  

  def appendSample = {
     Raz.path("a").append(Raz.path("b").path(pathVar[Int]).param("a", "a"))
     Raz.path("a").append(Raz.path("b").path(pathVar[Int]))
     Raz.path("a").append(Raz.path("b").path(pathVar[Int]).param(paramValueVar[Int]("ccc")))

    val u2 = Raz.path("a").path("b")
    
    val u3 = Raz / "a" / pathVar[String] / "b" / pathVar[Int] && ("a", "b") && paramValueVar[String]("aaa")
    val u4 = Raz && paramValueVar[String]("hhaaa")
    
    val u3a = u2.append(u3)
    val u3af = u3a.toF
    val u3am = u3a.matcher
    
    u3am( u3af("bb", 1 , "2") ) match {
       case Some(PathMatchResult(("bb", 1 , "2"), Path.empty)) => ()
       case x => throw new Exception(x.toString)
     }
    
   
     
    val u3b = u2.append(u3).append(u4)
    
    val u21 = Raz.path("a").param(paramValueVar[String]("aaa"))
    val u41 = Raz.param("yyyyera", "yeahhh").param(paramValueVar[String]("hhaaa")).param(paramValueVar[String]("hhaaaa")).param("YY", "XX").param(paramValueVar[String]("ccc"))
    
    val ur = u21.append(u41)
    
//    ur.toHTF
    
  }
  
  def htfSample = {
    val f1 = Raz.path("a").path("b").path(pathVar[Int]).param(paramValueVar[Int]("ccc")).toF

    println(f1(1, 2))
  }
  

  case class Cl1(a:Int,b:Int)
  
//  def implicitly[T](implicit t:T):T = t
  
  def matcherSample = {
    val u = Raz.path("a").path("b").path(pathVar[Int]).param(paramValueVar[Int]("ccc"))
//    val u = Raz.param(paramValueVar[Int]("bb")).param(paramValueVar[Int]("ccc"))
//    val f1:(Int,Int) => Path = u.toF(HF.fromHFAux(HFAux.hfaux2[Int,Int]))
    val f1:(Int,Int) => Path = u.toF
    
    f1(22, 33)
    val m1 = u.matcher
    
    val sc1 = Converter(Cl1.tupled, Cl1.unapply)

    val r:PathAndParamsHResource[RootPath, Cl1] = u.mapper.mapTo(sc1)

    val f2 = r.toF
    
    println(f2(Cl1(22,44)).render)
    println(r)
//    u.hello
    
    
    
    println(f1(1, 2))
    println(m1(f1(1, 2)))
  }
  
//  def matcherSample1 = {
//    val u = Raz.param(paramValueVar[Int]("bee")).param(paramValueVar[Int]("ccc"))
////    val u = Raz.param(paramValueVar[Int]("bb")).param(paramValueVar[Int]("ccc"))
////    val f1:(Int,Int) => Path = u.toF(HF.fromHFAux(HFAux.hfaux2[Int,Int]))
//    val f1:(Int,Int) => Path = u.toF
// 
//    val U1 = u
//    
//    u.toF.apply(21,22) match {
//      case U1.Matcher.Match((21, 22))  => ()
//      case _ => throw new Exception("")
//    }
//    
//    u.toF.apply(21,22) match {
//      case U1.Match((21, 22))  => ()
//      case _ => throw new Exception("")
//    }
//    
//    f1(22, 33)
//    val m1 = u.matcher
//    
//    val sc1 = Converter(Cl1.tupled, Cl1.unapply)
//
//    val r = u.mapper.mapTo(sc1)
//
//    val f2 = r.toF
//    
//    println(f2(Cl1(22,44)).render)
//    println(r)
////    u.hello
//    
//    
//    
//    println(f1(1, 2))
//    println(m1(f1(1, 2)))
//  }



  def utTest = {
    val u = Raz.path("a").path("b").path(pathVar[Int]).param(paramValueVar[Int]("ccc"))

    val ut = u.toUriTemplate
    
    println( ut.toF.apply("param1", "param2").render )
    println( ut.matcher.apply(ut.toF.apply("p1", "p2")) )
    
  }
  
  def angularUtTest = {
    val u = Raz.path("a").path("b").path(pathVar[Int]).param(paramValueVar[Int]("ccc")).param("par1", "2").param(paramValueVar[Int]("ddr"))

    val ut = u.toUriTemplate
    
    println( ut.toF.apply("param1", "param2", "param3").render )
    println( ut.matcher.apply(ut.toF.apply("p1", "p2", "p3")) )
    
  }
  
  def parSeqTest = {
    
    val u = Raz.path("a").path("b").path(pathVar[Int]).param(paramSeqVar[Int]("ccc"))
    val u1 = Raz.path("a").path("b").path(pathVar[Int]).param("a", "bba").param(paramSeqVar[Int]("ccc"))
    
    println( u.toF.apply(11, Seq(7,9,16)).render )
    println( u.matcher.apply(u.toF.apply(11, Seq(7,9,16))) )
    
    println( u.toUriTemplate.toF.apply("intPar", "intSeq").render)
    println( u1.toUriTemplate.toF.apply("intPar", "intSeq").render)
  }
  
   def optTest = {
    
    val u = Raz.path("a").path("b").path(pathVar[String]).param(optParamVar[Int]("opt"))
    
    val u1 = u.toF.apply("bbbb", Some(3))
    println( u.toF.apply("bbb", Some(3)).render )
    println( u.matcher.apply(u1) )
    
    assert(u.matcher.apply(u1).get.value == ("bbbb" -> Some(3)))
  }
   
   def noneTest = {

    val u = Raz.path("a").path("b").path(pathVar[String]).param(optParamVar[Int]("opt"))
    val u1 = u.toF.apply("bbbb", None)
 
    assert(u.matcher.apply(u1).get.value == ("bbbb" -> None))
    
    
  }
  
}