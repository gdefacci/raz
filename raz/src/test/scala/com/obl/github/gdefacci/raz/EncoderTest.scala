package com.github.gdefacci.raz

import org.scalatest.FunSuite
import org.scalatest.Matchers
import shapeless.HNil

class EncoderTest extends FunSuite with Matchers {
  
  import PathEncoder.{Segment, Param, Fragment}
  
  test("encode segment") {
    
    assert( Segment.string.encode("segment") === Path / "segment" )
    assert( Segment.int.encode(1) === Path / "1" )
    assert( Segment.boolean.encode(true) === Path / "true" )
    
  }

  test("encode param") {
    
    assert( Param("par").string.encode("value1") === (Path && ("par", "value1")) )
    assert( Param("par").int.encode(1) === (Path && ("par", "1")) )
    assert( Param("par").boolean.encode(true) === (Path && ("par", "true")) )
    
  }
  
  test("encode fragment") {
    
    assert( Fragment.string.encode("value1") === (Path &# "value1") )
    assert( Fragment.int.encode(1) === (Path &# "1") )
    assert( Fragment.boolean.encode(true) === (Path &# "true") )
    
  }
  
  lazy val sampleEncoder = {
    HTTP("www.syte.com", 8020) / "a" / Segment.string / "p" && ("par1", "value1") && Param("par2").int &# Fragment.boolean
  }
  
  test("encode path") {
    
    assert( sampleEncoder.pathEncoder.encode("segment1", 12, false) === (HTTP("www.syte.com", 8020) / "a" / "segment1" / "p" && ("par1", "value1") && ("par2", "12") &# "false"))
    
  }
  
  test("encoder contramap") {
    
    case class Cl1(a:String, b:Int, c:Boolean)
    
    val enc1 = sampleEncoder.pathEncoder.contramap(Cl1.unapply(_:Cl1).get)
    
    assert( enc1.encode(Cl1("segment1", 12, false)) === (HTTP("www.syte.com", 8020) / "a" / "segment1" / "p" && ("par1", "value1") && ("par2", "12") &# "false"))
    
  }
  
  test("encoder contramap 1") {
    val cn1 = (Segment.string / Segment.int).pathEncoder
    
    val cn2 = cn1 / Segment.boolean
    
    case class Cl1(a:(String,Int), c:Boolean)
    
    val enc2 = cn2.pathEncoder.contramap(Cl1.unapply(_:Cl1).get)
    
    assert(enc2.encode(Cl1("1" -> 2, true)) === (Path / "1" / "2" / "true"))
    
    val cn3 = Path / "a" / cn1 / Segment.boolean
    
    val enc3 = cn3.pathEncoder.contramap(Cl1.unapply(_:Cl1).get)
    
    assert(enc3.encode(Cl1("1" -> 2, true)) === (Path / "a" / "1" / "2" / "true"))
    
    case class Cl2(a:(String,Int))
    
    val cn4 = Path / cn1 
    val cn4a = HTTP("home.com") / cn1 / Segment.boolean 
    val cn4b = HTTP("home.com") / cn1
    
    val enc4a = cn4a.pathEncoder.contramap(Cl1.unapply(_:Cl1).get)
    val enc4b = cn4b.pathEncoder.contramap(Cl2.unapply(_:Cl2).get)
    val enc4 = cn4.pathEncoder.contramap(Cl2.unapply(_:Cl2).get)
    
    assert(enc4.encode(Cl2("1" -> 2)) === (Path / "1" / "2"))
    assert(enc4a.encode(Cl1("1" -> 2, true)) === (HTTP("home.com") / "1" / "2" / "true"))
    assert(enc4b.encode(Cl2("1" -> 2)) === (HTTP("home.com") / "1" / "2"))
    
  }

  
  test("encoder append") {
    
    assert( (Segment.string / "sg2").encode("segment") === (Path / "segment" / "sg2"))
    assert( (Segment.string && "sg2").encode("segment") === (Path / "segment" && "sg2"))
    
    """PathEncoder.Param.string.append(Path / "sg2")""" shouldNot compile
    
    assert( (Param("par").string && "sg2").encode("value1") === (Path && ("par", "value1") && "sg2"))
    
    """PathEncoder.Fragment.string / "sg2"""" shouldNot compile
    """PathEncoder.Fragment.string && "sg2"""" shouldNot compile
    """PathEncoder.Fragment.string &# "sg2"""" shouldNot compile
    
  }
  
  test("path render") {
   
    def checkSymmetric(p:Path) = assert(Path.fromJavaUrl(new java.net.URI(p.render).toURL() ).toOption.get == p)
    
    val p2 = Path(Some(HTTP), Some(Authority("uSLn.com",3)),List("y", "Bz%8", "(GD~", "s:iR"), List(("ky&",Some("rL"))),Some("F"))
    val p1 = Path(Some(HTTP), Some(Authority("uSLn.com",3)),List("y", "a/b"), List(("ky&",Some("rL"))),Some("F"))
    
    println(p1.render)
    println(new java.net.URI(p1.render).toURL())
    checkSymmetric( p1 )
  }
  

}

