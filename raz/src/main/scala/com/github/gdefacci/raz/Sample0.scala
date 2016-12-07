package com.github.gdefacci.raz

object Sample0 extends App {
  
  val p1 = Path.empty[PathPosition.Segment] / "" / "" / "" && "" && ("", "")
  val p1s = Path.empty[PathPosition.Segment] && "" && ("", "")

  val pec1:PathEncoder[String, PathPosition.Segment, PathPosition.Segment] = PathEncoder.Segment.string / "" / "" / ""
  val pec1s:PathEncoder[String, PathPosition.Segment, PathPosition.Param] = PathEncoder.Segment.string / "" / "" / "" && ""
  
  val pec2:PathEncoder[String, PathPosition.Segment, PathPosition.Segment] = Path.segments("a", "a") / PathEncoder.Segment.string 
  val pec2s:PathEncoder[String, PathPosition.Segment, PathPosition.Param] = Path.segments("a", "a") && PathEncoder.Param("parName").string 
  val pec2F:PathEncoder[String, PathPosition.Segment, PathPosition.Fragment] = Path.segments("a", "a") && PathEncoder.Param("parName").string &# ""  
  val hpec1 = (Path / "a" / "a" / PathEncoder.Segment.string / "" / PathEncoder.Segment.string) &# "" 
  
  {
  
  val hpec1P = Path / PathEncoder.Segment.string / PathEncoder.Segment.string && PathEncoder.Param("parName").string
  val hpec1P1A = Path / PathEncoder.Segment.string / PathEncoder.Segment.string / "" / PathEncoder.Segment.string / "" / PathEncoder.Segment.string / "" / PathEncoder.Segment.string / ""
  val hpec1P1 = Path / PathEncoder.Segment.string / PathEncoder.Segment.string && ""
  val hpec1P1AA = Path / PathEncoder.Segment.string / PathEncoder.Segment.string / "" && "" && PathEncoder.Param("").string && "" && ("", "")
  val hpec1P2 = Path / PathEncoder.Segment.string / PathEncoder.Segment.string && ("", "")
  
  }
  val hpec2 = Path.segments("a", "a") / PathEncoder.Segment.string / "b" / PathEncoder.Segment.string / PathEncoder.Segment.string    
  
  {
    val r1Enc = PathEncoder.Segment.string / "b" / PathEncoder.Segment.string / PathEncoder.Segment.string 
    val r1EncP = PathEncoder.Segment.string / "b" / PathEncoder.Segment.string && PathEncoder.Param("par").string 
    val r1 = PathDecoder.Segment.string / "b" / PathDecoder.Segment.string / PathDecoder.Segment.string 
    val r1P = PathDecoder.Segment.string / "b" / PathDecoder.Segment.string && PathDecoder.Param("par").string
    val rConv = PathConverter.Segment.string / "b" / PathConverter.Segment.string / PathConverter.Segment.string 
    val rConvP = PathConverter.Segment.string / "b" / PathConverter.Segment.string / PathConverter.Segment.string && PathConverter.Param("par").string 

    println( r1.pathDecoder.decodeFull(r1Enc.pathEncoder.encode("a", "b", "c")) )
    println( r1P.pathDecoder.decodeFull(r1EncP.pathEncoder.encode("a", "b", "c")) )
    println( rConv.pathConverter.decodeFull(rConv.pathConverter.encode("a", "b", "c")) )
    println( rConvP.pathConverter.decodeFull(rConvP.pathConverter.encode("a", "b", "c", "d")) )
    println( rConv.pathDecoder.decodeFull(rConv.pathEncoder.encode("a", "b", "c")) )
    println( rConvP.pathDecoder.decodeFull(rConvP.pathEncoder.encode("a", "b", "c", "d")) )
  }
  
  {
    val wr1 = PathConverter.Segment.string / "b" && PathConverter.Param("par").string
    val wr2 = PathConverter.Segment.string / "b" && PathConverter.Param("par").string && PathConverter.Param("").string  
    
  }
  
  println( hpec2.pathEncoder.encode("1", "2", "3").render )
}