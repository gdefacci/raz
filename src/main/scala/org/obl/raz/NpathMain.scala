package org.obl.raz

object NpathMain {

  def main(args: Array[String]): Unit = {
    val p1 = Raz / "a" / "b" / "c"

    val p2 = (p1 && ("a", "a") && ("b", "b")) ## "a"

    val p3 = Raz ## ""
    val p3a = Raz && ("", "") && ("", "")

    val p4 = p1 ++ p3
    val p4a = p1 ++ p3a
    val p4b = p1 ++ p1

    //    p3 ++ p1 // shuold not compile
    //    p3a ++ p1 // shuold not compile
    //    p3 ++ p3a // shuold not compile

    val p5a = p3a ++ p3
    val p5b = p1 ++ p1 ++ p3a ++ p3

    //    p3.at("www.pippo.com").addFragment("dd")  // shuold not compile
    (p3a at "www.pippo.com") ## "dd"
    p3a ## "dd" at "www.pippo.com"

    testHPath
    
    testHPath_concat_2
    
    testMatchPath

    test_map
    test_ut
  }

  def testHPath = {


    val p1 = Raz / "bubu" / pathVar[Int] / "bb" && ("aa", "17") && paramValueVar[String]("name") && paramValueVar[Int]("name1")

    //    val aux = HPathF.toHPathF2[IsRelativePath, CanAddParam, CanHavePathAsPrefix,String,String]
    //    
    println((p1(123, "", 12) ## "frag1" at "mysite.com").render)
    assert("mysite.com/bubu/123/bb?aa=17&name=&name1=12#frag1" == (p1.apply(123, "", 12) ## "frag1" at "mysite.com").render)

  }

  def testHPath_concat_1 = {


    val p1 = Raz / "bubu" / pathVar[Int] / pathVar[String]
    val p1par = Raz && paramValueVar[Int]("p1")
    val p1a = HPathNil(Raz / "bubu")
    val p1b = HPathNil(Raz && ("bubu", "bibi"))

    val p2a = p1.concat(p1a)
    val p2b = p1.concat(p1b)
    
    val p2aa = p1a.concat(p1)
//    val p2ab = p1b.concat(p1)   // should not compile
//    val p2ac = p1b.concat(p1par )   // should not compile

    val pq1 = Raz / "bubu" && paramValueVar[String]("name")
    val pq1a = HPathNil(Raz / "bubu")
    val pq1b = HPathNil(Raz && ("bubu", "bibi"))

    //    val pq2a = pq1.concat(pq1a) // should not compile
    val pq2b = pq1.concat(pq1b)

  }

  def testHPath_concat_2 = {

    val p1 = Raz / "bubu" / pathVar[Int] / pathVar[String] / pathVar[Int] / pathVar[String]
    val p1a = HPathNil(Raz / "bubu") 
    val p1b = Raz / "p1" && ("bubu", "bibi") && paramValueVar[String]("name")
    val p1c = Raz && ("par1", "1111") && paramValueVar[String]("par2") && ("", "")

    val p2a = p1.concat(p1a)
    val p2b = p1.concat(p1b)
    val p2c = p1b.concat(p1c)
    
//    p1b .concat(p1)	// should not compile
    
    println( p2b(12, "p1", 12, "p1", "nmValue").render )

  }
  
  def test_concat_3 = {
    
    val u3 = Raz && paramValueVar[String]("aaa")
    val u4 = Raz && paramValueVar[String]("hhaaa")
    
    u3.concat(u4)//(HAppend.hPathCons_HPathCons2(HAppend.hPathCons_HPathNil2))
    
  }
  
  def test_concat3 = {
    val u21 = Raz.add("a").addParam(paramValueVar[String]("aaa"))
    val u41 = Raz.addParam("yyyyera", "yeahhh").addParam(paramValueVar[String]("hhaaa")).addParam(paramValueVar[String]("hhaaaa")).addParam("YY", "XX").addParam(paramValueVar[String]("ccc"))

    val ur = u21.concat(u41)
  }

   def testMatchPath = {
	  val p1 = Raz / "bubu" / pathVar[Int] / pathVar[String]
	  val pt1 = p1(12, "bubu")
     
	  val res = p1.matchPath(pt1)
	  
	  val res1:Option[PathMatchResult[(Int,String), Path]] = res
	  
	  p1.matchPath(pt1) match {
       case Some(PathMatchResult(pair,rest)) => {
         val (p1:Int, p2:String) = pair
         assert(p1 == 12)
         assert(p2 == "bubu")
         assert(rest.isEmpty)
       }
       case _ => ???
     }
     
	  p1.matchPath(pt1) match {
       case Some(PathMatchResult(Pair(12, "bubu"),rest)) if rest.isEmpty => ()
       case _ => ???
     }
     
   }
   
   
   case class Cl1(a:Int)
   case class Cl2(a:Int, b:String)
   def test_map = {
	  val p1 = Raz / "bubu" / pathVar[Int] 
	 
     val cnv = Converter(Cl1, Cl1.unapply)
//     p1.mapTo(cnv)(HMapper.mapper1)
     val r1 = p1.mapTo(cnv)
     
     println( r1(Cl1(17)).render )
 
     val p2 = Raz / "bubu" / pathVar[Int] / pathVar[String]

     val cnv2 = Converter(Cl2.tupled, Cl2.unapply)
     
     val r2 = p2.mapTo(cnv2)
     
     println( r2(Cl2(17, "strprop")).render )
   }
   
   
   def test_ut = {
	  val p1 = Raz / "bubu" / pathVar[Int] / "dudu" / pathVar[Int] && paramValueVar[String]("par") 
	 
	  
	  println( p1.toUriTemplate("s", "dd", "parVAlue") )
     
   }
}