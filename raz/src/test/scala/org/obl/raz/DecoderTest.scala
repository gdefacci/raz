package org.obl.raz

import org.scalatest.FunSuite
import org.scalatest.Matchers

import scalaz.{ -\/, \/, \/- }

class DecoderTest extends FunSuite with Matchers {

  import PathDecoder.{ Segment, Param, Fragment }

  test("decode segment") {

    assert(Segment.string.decodeFull(Path / "segment") == \/-("segment"))
    assert(Segment.int.decodeFull(Path / "1") == \/-(1))
    assert(Segment.boolean.decodeFull(Path / "true") == \/-(true))

  }

  test("decode param") {

    val par = "par"
    assert(Param(par).string.decodeFull(Path && (par, "v1")) == \/-("v1"))
    assert(Param(par).int.decodeFull(Path && (par, "1")) == \/-(1))
    assert(Param(par).boolean.decodeFull(Path && (par, "true")) == \/-(true))

  }

  test("decode fragment") {

    assert(Fragment.string.decodeFull(Path &# "v1") == \/-("v1"))
    assert(Fragment.int.decodeFull(Path &# "1") == \/-(1))
    assert(Fragment.boolean.decodeFull(Path &# "true") == \/-(true))

  }

  test("decode params") {

    val dec1 = Param("p1").string && Param("p2").int

    assert(dec1.pathDecoder.decodeFull(Path && ("p2", "1") && ("p1", "2")) == \/-("2" -> 1))
    assert(dec1.pathDecoder.decodeFull(Path && ("p1", "2") && ("p2", "1")) == \/-("2" -> 1))
  }

  test("decode segment append") {

    assert((Segment.string / "a").decodeFull(Path / "segment" / "a") == \/-("segment"))
    assert((Segment.string && "a").decodeFull(Path / "segment" && "a") == \/-("segment"))
    assert((Segment.string &# "a").decodeFull(Path / "segment" &# "a") == \/-("segment"))

  }


  test("decode param append") {

    val par = "par"

    """Param(par).string / "a"""" shouldNot compile
    assert((Param(par).string && "a").decodeFull(Path && (par, "v1") && "a") == \/-("v1"))
    assert((Param(par).string &# "a").decodeFull(Path && (par, "v1") &# "a") == \/-("v1"))

  }


  test("decode fragment append") {
    """Fragment.string / "a"""" shouldNot compile
    """Fragment.string && "a"""" shouldNot compile
    """Fragment.string &# "a"""" shouldNot compile
  }


  lazy val sampleDecoder = HTTP("www.site.com", 8020) / "a" / Segment.string / "p" && ("par1", "value1") && Param("par2").int &# Fragment.boolean
  lazy val sampleDecoder1 = Path / "a" / Segment.string / "p" && ("par1", "value1") && Param("par2").int &# Fragment.boolean

  test("decode path") {

    val p1 = (Path / "a" / "p_1" / "p" && ("par1", "value1") && ("par2", "2") &# "true")

    assert(sampleDecoder1.pathDecoder.decodeFull(p1) == \/-(("p_1", 2, true)))

  }
  
  test("decode map") {

    val p1 = (Path / "a" / "p_1" / "p" && ("par1", "value1") && ("par2", "2") &# "true")

    case class Cl1(a:String, b:Int, c:Boolean)
    
    val sdec = sampleDecoder1.pathDecoder.map(Cl1.tupled)
    
    assert(sdec.decodeFull(p1) == \/-(Cl1("p_1", 2, true)))

  }
  
  test("decoder map 1") {
    import PathDecoder.{Segment, Param, Fragment}
    
    val cn1 = (Segment.string / Segment.int).pathDecoder
    
    val cn2 = cn1 / Segment.boolean
    val cn2a = Segment.string / Segment.int / Segment.boolean
    
    case class Cl1(a:(String,Int), c:Boolean)
    
    val enc2 = cn2.pathDecoder.map(Cl1.tupled)
    val enc2a = cn2a.pathDecoder.map( p => Cl1(p._1 -> p._2, p._3))
    
    assert(enc2a.decodeFull((Path / "1" / "2" / "true")) == \/-(Cl1("1" -> 2, true)))
    assert(enc2.decodeFull((Path / "1" / "2" / "true")) == \/-(Cl1("1" -> 2, true)))
    
    val cn3 = Path / "a" / cn1 / Segment.boolean
    
    val enc3 = cn3.pathDecoder.map(Cl1.tupled)
    
    assert(enc3.decodeFull(Path / "a" / "1" / "2" / "true") == \/-(Cl1("1" -> 2, true)))
    
    case class Cl2(a:(String,Int))
    
    val cn4 = Path / cn1 
    val cn4a = HTTP("home.com") / cn1 / Segment.boolean 
    val cn4b = HTTP("home.com") / cn1
    
    val enc4a = cn4a.pathDecoder.map(Cl1.tupled)
    val enc4b = cn4b.pathDecoder.map(Cl2.apply(_))
    val enc4 = cn4.pathDecoder.map(Cl2.apply(_))
    
    assert(enc4.decodeFull(Path / "1" / "2") == \/-(Cl2("1" -> 2)))
    assert(enc4a.decodeFull(HTTP("home.com") / "1" / "2" / "true") ==  \/-(Cl1("1" -> 2, true)))
    assert(enc4b.decodeFull(HTTP("home.com") / "1" / "2" ) == \/-((Cl2("1" -> 2))))
    
  }

  
  test("decode orElse") {

    val dec1 = Segment.string / "b" && Param("par1").int
    
    val dec2 = Path / "a" / Segment.string / "c" && Param("par2").int
    
    val dec = dec1.pathDecoder.orElse(dec2.pathDecoder)
    
    val p1 = (Path / "a" / "p_1" / "c" && ("par2", "2"))
    val p2 = (Path / "p_1" / "b" && ("par1", "2"))

    assert(dec.decodeFull(p1) == \/-(("p_1", 2)))
    assert(dec.decodeFull(p2) == \/-(("p_1", 2)))

  }
  
  test("decode path at") {
    
    val p0 = HTTP("www.site.com", 8020) / "a"
    
    def p1Path[S <: PathPosition] = ((_:TPath[S, PathPosition.Segment]) / "p_1" / "p" && ("par1", "value1") && ("par2", "2") &# "true")
    
    val p1 = p1Path(Path.empty)
    val p1Full = p1Path(p0)
    
    val dec1 = sampleDecoder.pathDecoder.decoderAt(p0)
    
    assert(dec1.decodeFull(p1) == \/-(("p_1", 2, true)))
    assert(dec1.fullPath.decodeFull(p1Full) == \/-(("p_1", 2, true)))
    
    
  }
  
  test("path subtract") {
    
    def subtract(from:Path, what:Path):Throwable \/ Path  = 
       PathDecoder.path(what).decode(from).map( mr => mr.rest )
    
    {
      val pa1 = Path / "a" / "b" / "c"
      val pa2 = Path / "a" 
      
      assert(subtract(pa1, pa2) == \/-(Path / "b" / "c"))
    }
    
    {
      val pa1 = Path / "a" / "b" / "c" && ("p", "2")
      val pa2 = Path / "a" 
    
      assert(subtract(pa1, pa2) == \/-(Path / "b" / "c" && ("p", "2")))
    }    
    
    {
      val pa1 = Path / "a" && ("p", "2") && ("p1", "3")
      val pa2 = Path / "a" && ("p1", "3")
    
      assert(subtract(pa1, pa2) == \/-(Path && ("p", "2")))
    }    
    
  }
  
}