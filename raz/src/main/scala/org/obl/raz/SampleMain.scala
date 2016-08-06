package org.obl.raz

import shapeless._

object SampleMain {
  
  Path / "a" / "b" && "pippo" &# "rocky"

  HTTP("www.google.com") / "a" && ("arg" , "12")
  
}

object EncSample  {
  
  import PathEncoder.{Segment, Param, Fragment}
  
  case class Cl2(a:String, b:Boolean, c:Int)
   
  val d1 = (Segment.string / "aaa" / Segment.boolean / "bb" / Segment.int ).pathEncoder.contramap( (cl:Cl2) => Cl2.unapply(cl).get )
  
  assert(d1.encode(Cl2("a", true, 12)) == Path(None, None, List("a", "aaa", "true", "bb", "12"), Nil, None))
  
  val d2 = (Segment.string / "aaa" / Segment.boolean / "bb" / Segment.int && "pippo" && ("pluto", "18")). pathEncoder.contramap( (cl:Cl2) => Cl2.unapply(cl).get )
  
  assert(d2.encode(Cl2("a", true, 12)) == Path(None, None, List("a", "aaa", "true", "bb", "12"), List(("pippo" -> None), ("pluto" -> Some("18"))), None))
  
  val d3 = Segment.string && Param("b1").boolean && Param("pluto").int //.pathEncoder.contramap( (cl:Cl2) => Cl2.unapply(cl).get )
  val d3h = Segment.string :: Param("b1").boolean :: Param("pluto").int :: HNil
  
  case class Cl3(a:String, a1:String, b:Boolean, c:Int)
  
  println( (Path / "pippo" / Segment.string / "sg1" / Segment.string / "sg2" && Param("b1").boolean && Param("pluto").int &# "bah").pathEncoder.contramap( (cl:Cl3) => Cl3.unapply(cl).get ).encode(Cl3("yea", "aah", true, 7777)) )
  
  case class Cl1(name:String, minni:Int, pippo:Int)
  
  HTTP("www.google.com") / Segment.string
  
  val inst = Cl1("a string", 2, 1)
  val pth1:TPath[PathPosition.Segment, PathPosition.Param] = (Segment.string :: Param("pippo").int :: Param("pippo").int :: HNil).contramap( (c:Cl1) => Cl1.unapply(c).get).encode(inst)
  
  println( (Segment.string :: Param("minni").int :: Param("pippo").int :: HNil).contramap( (c:Cl1) => Cl1.unapply(c).get).encode(inst) )
  
  (Segment.string / "yeah" / Segment.string && Param("minni").int && "single-param" && Param("minni").int && ("param", "value") &# Fragment.string).pathEncoder.contramap( (a:Int) => ??? )
  
  case class Cl2a(a:String, b:String, c:String)
  
  (Segment.string / "yeah" / Segment.string && Param("minni").string).pathEncoder.contramap( (cl:Cl2a) => Cl2a.unapply(cl).get)
  
  
  val ei1 = Segment.string  / "aa" 
  val ei2 = Segment.string / "aa" && Param("pippo").string
  
  val ei3 = (ei1 ++ ei2).pathEncoder.contramap( (cl:Cl2a) => Cl2a.unapply(cl).get)
  
  println(ei3.encode(Cl2a("a", "b", "c")))
  
  assert(ei3.encode(Cl2a("a", "b", "c")) == (Path / "a" / "aa" / "b" / "aa" && ("pippo", "c")))
}

object DecSample extends App {

  import PathDecoder.{Segment, Param}

  case class Cl3(a:String, a1:String, b:Int, c:Int)
  
  val p1 = Path / "field a" / "yeah" / "field a1" && ("minni", "177") && ("minni", "172") && "single-param" && ("param", "value")
  val r = (Segment.string / "yeah" / Segment.string && Param("minni").int && "single-param" && Param("minni").int && ("param", "value")).pathDecoder.map( Cl3.tupled ).decode(p1)
  println(r)
  case class Cl1(name:String, minni:Int, pippo:Int)
  
  (Segment.string / Segment.int && Param("pippo").int).pathDecoder.map(Cl1.tupled)
}


object UriTemplateSample {
  
  import UriTemplate.PlaceHolder
  
  println( UriTemplate / "aa" / PlaceHolder("bb") && ("pippo", PlaceHolder("c")) &# "zed" )
  
}

object CodecSample {
  
  import PathCodec.{Segment, Param}
  
  (Segment.string :: Segment.boolean :: HNil).pathCodec.encoder
  
  (Segment.string :: Segment.boolean :: HNil).pathCodec.decoder
  
  (Segment.string :: Segment.boolean :: HNil).pathCodec

  (Segment.string / Segment.boolean / "sg1" && Param("b").int).pathCodec 
}

object PathConverterSample {
  
  import PathConverter.{Segment, Param, Fragment}
  
  
  (Segment.string :: Segment.string :: Param("pippo").int :: HNil).pathConverter
  
  println(
    (Path / "pippo" / Segment.string / "sg1" / Segment.string / "sg2" && Param("b1").boolean && Param("pluto").int &# "bah").pathConverter.uriTemplateEncoder.encodeUriTemplate("S_1", "S_2", "P_1", "P_2")
  )
}