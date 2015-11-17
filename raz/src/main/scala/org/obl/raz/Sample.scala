package org.obl.raz

import PathConverter._

object Sample extends App {

////    matcherSample
    appendSample
    utTest
    parSeqTest
    optTest
    noneTest 
//    test_#
//    
//    test1_# 
  
//  def typeOf[T](t:T)(implicit ct:Manifest[T]) = ct

  
  def appendSample = {
     RelativePath.add("a").append(RelativePath.add("b").add(Segment.int).addParam("a", "a"))
     RelativePath.add("a").append(RelativePath.add("b").add(Segment.int))
     RelativePath.add("a").append(RelativePath.add("b").add(Segment.int).addParam(Param.int("ccc")))

    val u2 = RelativePath.add("a").add("b")
    
    val u3 = RelativePath / "a" / Segment.string / "b" / Segment.int && ("a", "b") && Param.string("aaa")
    val u4 = RelativePath && Param.string("hhaaa")
    
    val u3a = u2.append(u3)
    
    u3a.decodeFull( u3a("bb", 1 , "2") ).toOption match {
       case Some(("bb", 1 , "2")) => ()
       case x => throw new Exception(x.toString)
     }
    
   
    val u3b = u2.append(u3).concat(u4)
    val u21 = RelativePath.add("a").addParam(Param.string("aaa"))
    val u41 = RelativePath.addParam("yyyyera", "yeahhh").addParam(Param.string("hhaaa")).addParam(Param.string("hhaaaa")).addParam("YY", "XX").addParam(Param.string("ccc"))
    
    val ur = u21.concat(u41)
    
    
  }
  
  def htfSample = {
    val f1 = RelativePath.add("a").add("b").add(Segment.int).addParam(Param.int("ccc"))

    println(f1(1, 2))
  }
  

//  final case class Cl1(a:Int,b:Int)
//  
//  
//
//
  def utTest = {
    val u = RelativePath.add("a").add("b").add(Segment.int).addParam(Param.int("ccc"))

    u(1,1)
    
    println( u.toUriTemplate("param1", "param2").render )
    
  }
  
  def parSeqTest = {
    
    val u = RelativePath.add("a").add("b").add(Segment.int).addParam(PathConverter.seq(Param.int("ccc")))
    val u1 = RelativePath.add("a").add("b").add(Segment.int).addParam("a", "bba").addParam(PathConverter.seq(Param.int("ccc")))
    
    println( u(11, Seq(7,9,16)).render )
    println( u.decodeFull(u(11, Seq(7,9,16))) )
    
    println( u.toUriTemplate("intPar", "intSeq").render)
  }
  
   def optTest = {
    
    val u = RelativePath.add("a").add("b").add(Segment.string).addParam(PathConverter.opt(Param.int("opt")))
    
    val u1 = u("bbbb", Some(3))
    println( u("bbb", Some(3)).render )
    
    assert(u.decodeFull(u1).toOption.get == ("bbbb" -> Some(3)))
  }
   
   def noneTest = {

    val u = RelativePath.add("a").add("b").add(Segment.string).addParam(PathConverter.opt(Param.int("opt")))
    val u1 = u("bbbb", None)
 
    assert(u.decodeFull(u1).toOption.get == ("bbbb" -> None))
    
    
  }
//   
   def test_# = {
     
	 val hp1 = RelativePath / "a"  &# "myfragment" 
     val hp2 = RelativePath / "a" && ("par", "1") &# "myfragment" 
     
     println(hp1.render)
     assert("/a#myfragment" == hp1.render)
     assert("/a?par=1#myfragment" == hp2.render)
     
     val u = RelativePath.add("a").add("b").add(Segment.int).addParam(Param.int("ccc")).addFragment("myfrag")
     
     val hp3 = u.apply(1,2)
     
     assert("/a/b/1?ccc=2#myfrag" == hp3.render)
 
     val up1 = RelativePath.add("a").add("b").add(Segment.int).add(Segment.int).addFragment("f")
     val utp1 = RelativePath.add("a").add("b").add(Segment.int).add(Segment.int).addFragment("f").toUriTemplate("a","b")
     
     println(up1(11,22).render)
     println(utp1.render)
     assert("/a/b{/a}{/b}#f" == utp1.render)
     
     val utp2 = u.toUriTemplate("sym1", "sym2").render
     
     println(utp2)
     
     assert("/a/b{/sym1}?ccc={sym2}#myfrag" == utp2)
   }
 
   def test1_# = {
     

     val utp1 = RelativePath.add("a").add("b").add(Segment.int).add(Segment.int).addFragment("f").toUriTemplate("a","b").render
     
     println(utp1)
     assert("/a/b{/a}{/b}#f" == utp1)
     
   }
   
   final case class Cl2(a:Int, b:String, c:Boolean)
   
  def testRootPath = {
    val u1 = RelativePath / "a" / Segment.int / "b" / Segment.string / Segment.boolean
    val up1 = RelativePath / "bbb" && ("a", "bb") && Param.int("par1") && ("b", "bbb") && Param.string("par2") && Param.boolean("par3")
    
    println(u1(10, "abba", true))
    println(u1(10, "abba", true).render)
    println(u1.decodeFull( u1(10, "abba", true) ))
    
//    var sc = Converter.tryConverter(Cl1.tupled, Cl1.unapply)
    
    val u2 = PathConverter(u1).caseMap(Cl2.tupled, Cl2.unapply) 
    val up2 = PathConverter(up1).caseMap(Cl2.tupled, Cl2.unapply) 

    RelativePath / up2
    RelativePath / u2 / up2 
//    check(u1, (10, "abba", true))
//    check(Raz / u2, Cl1(10, "abba", true))
    
  }

}