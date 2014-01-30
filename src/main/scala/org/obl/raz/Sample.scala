package org.obl.raz

object Sample {

  def main(args:Array[String]):Unit =  {
    matcherSample
    appendSample
    utTest
    parSeqTest
    optTest
    noneTest 
    test_#
    
//    test1_# 
  }
  
  import PathFs._
  
  def typeOf[T](t:T)(implicit ct:Manifest[T]) = ct

  def appendSample = {
     Raz.add("a").append(Raz.add("b").add(pathVar[Int]).addParam("a", "a"))
     Raz.add("a").append(Raz.add("b").add(pathVar[Int]))
     Raz.add("a").append(Raz.add("b").add(pathVar[Int]).addParam(paramValueVar[Int]("ccc")))

    val u2 = Raz.add("a").add("b")
    
    val u3 = Raz / "a" / pathVar[String] / "b" / pathVar[Int] && ("a", "b") && paramValueVar[String]("aaa")
    val u4 = Raz && paramValueVar[String]("hhaaa")
    
    val u3a = u2.append(u3)
//    val u3af = u3a.toF
//    val u3am = u3a.matcher
    
    u3a.matchPath( u3a("bb", 1 , "2") ) match {
       case Some(PathMatchResult(("bb", 1 , "2"), pth)) if pth.isEmpty => ()
       case x => throw new Exception(x.toString)
     }
    
   
//     u3 concat u4
//     
    val u3b = u2.append(u3).concat(u4)
//    
    val u21 = Raz.add("a").addParam(paramValueVar[String]("aaa"))
    val u41 = Raz.addParam("yyyyera", "yeahhh").addParam(paramValueVar[String]("hhaaa")).addParam(paramValueVar[String]("hhaaaa")).addParam("YY", "XX").addParam(paramValueVar[String]("ccc"))
    
    val ur = u21.concat(u41)
    
    
  }
  
  def htfSample = {
    val f1 = Raz.add("a").add("b").add(pathVar[Int]).addParam(paramValueVar[Int]("ccc"))

    println(f1(1, 2))
  }
  

  case class Cl1(a:Int,b:Int)
  
//  def implicitly[T](implicit t:T):T = t
  
  def matcherSample = {
    val u = Raz.add("a").add("b").add(pathVar[Int]).addParam(paramValueVar[Int]("ccc"))
//    val u = Raz.param(paramValueVar[Int]("bb")).param(paramValueVar[Int]("ccc"))
//    val f1:(Int,Int) => Path = u.toF(HF.fromHFAux(HFAux.hfaux2[Int,Int]))
    
    u(22, 33)
//    val m1 = u.matcher
    
    val sc1 = Converter(Cl1.tupled, Cl1.unapply)

    val r = u.mapTo(sc1)

    val f2 = r
    
    println(f2(Cl1(22,44)).render)
    println(r)
//    u.hello
    
    
    
    println(u(1, 2))
    println(u.matchPath(u(1, 2)))
  }
  
////  def matcherSample1 = {
////    val u = Raz.param(paramValueVar[Int]("bee")).param(paramValueVar[Int]("ccc"))
//////    val u = Raz.param(paramValueVar[Int]("bb")).param(paramValueVar[Int]("ccc"))
//////    val f1:(Int,Int) => Path = u.toF(HF.fromHFAux(HFAux.hfaux2[Int,Int]))
////    val f1:(Int,Int) => Path = u.toF
//// 
////    val U1 = u
////    
////    u.toF.apply(21,22) match {
////      case U1.Matcher.Match((21, 22))  => ()
////      case _ => throw new Exception("")
////    }
////    
////    u.toF.apply(21,22) match {
////      case U1.Match((21, 22))  => ()
////      case _ => throw new Exception("")
////    }
////    
////    f1(22, 33)
////    val m1 = u.matcher
////    
////    val sc1 = Converter(Cl1.tupled, Cl1.unapply)
////
////    val r = u.mapper.mapTo(sc1)
////
////    val f2 = r.toF
////    
////    println(f2(Cl1(22,44)).render)
////    println(r)
//////    u.hello
////    
////    
////    
////    println(f1(1, 2))
////    println(m1(f1(1, 2)))
////  }



  def utTest = {
    val u = Raz.add("a").add("b").add(pathVar[Int]).addParam(paramValueVar[Int]("ccc"))

    u(1,1)
//    val ut = u.toUriTemplate("par1","par2")
    
    println( u.toUriTemplate("param1", "param2").render )
    
  }
//  
//  def angularUtTest = {
//    val u = Raz.path("a").path("b").path(pathVar[Int]).param(paramValueVar[Int]("ccc")).param("par1", "2").param(paramValueVar[Int]("ddr"))
//
//    val ut = u.toUriTemplate
//    
//    println( ut.toF.apply("param1", "param2", "param3").render )
//    println( ut.matcher.apply(ut.toF.apply("p1", "p2", "p3")) )
//    
//  }
//  
  def parSeqTest = {
    
    val u = Raz.add("a").add("b").add(pathVar[Int]).addParam(paramSeqVar[Int]("ccc"))
    val u1 = Raz.add("a").add("b").add(pathVar[Int]).addParam("a", "bba").addParam(paramSeqVar[Int]("ccc"))
    
    println( u(11, Seq(7,9,16)).render )
    println( u.matchPath(u(11, Seq(7,9,16))) )
    
    println( u.toUriTemplate("intPar", "intSeq").render)
  }
  
//  
   def optTest = {
    
    val u = Raz.add("a").add("b").add(pathVar[String]).addParam(optParamVar[Int]("opt"))
    
    val u1 = u("bbbb", Some(3))
    println( u("bbb", Some(3)).render )
//    println( u.matcher.apply(u1) )
    
    assert(u.matchPath(u1).get.value == ("bbbb" -> Some(3)))
  }
   
   def noneTest = {

    val u = Raz.add("a").add("b").add(pathVar[String]).addParam(optParamVar[Int]("opt"))
    val u1 = u("bbbb", None)
 
    assert(u.matchPath(u1).get.value == ("bbbb" -> None))
    
    
  }
   
   def test_# = {
     
	 val hp1 = (Raz / "a" ) ## "myfragment" 
     val hp2 = (Raz / "a" && ("par", "1")) ## "myfragment" 
     
     println(hp1.render)
     assert("/a#myfragment" == hp1.render)
     assert("/a?par=1#myfragment" == hp2.render)
     
     val u = Raz.add("a").add("b").add(pathVar[Int]).addParam(paramValueVar[Int]("ccc")).addFragment("myfrag")
     
     val hp3 = u.apply(1,2)
     
     assert("/a/b/1?ccc=2#myfrag" == hp3.render)
 
     val up1 = Raz.add("a").add("b").add(pathVar[Int]).add(pathVar[Int]).addFragment("f")
     val utp1 = Raz.add("a").add("b").add(pathVar[Int]).add(pathVar[Int]).addFragment("f").toUriTemplate("a","b")
     
     println(up1(11,22).render)
     println(utp1.render)
     assert("/a/b/{a}/{b}#f" == utp1.render)
     
     val utp2 = u.toUriTemplate("sym1", "sym2")
     
     assert("/a/b/{sym1}?ccc={sym2}#myfrag" == utp2.render)
   }
 
   def test1_# = {
     

     val utp1 = Raz.add("a").add("b").add(pathVar[Int]).add(pathVar[Int]).addFragment("f").toUriTemplate("a","b")
     
     println(utp1.render)
     assert("/a/b/{a}/{b}#f" == utp1.render)
     
   }
}