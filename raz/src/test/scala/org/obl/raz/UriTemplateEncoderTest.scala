package org.obl.raz

import org.scalatest.FunSuite
import org.obl.raz.UriTemplate.PlaceHolder
import org.obl.raz.UriTemplate.PlaceHolder

class UriTemplateEncoderTest extends FunSuite {

  import UriTemplateEncoder._

  import UriTemplate.{ PlaceHolder }

  test("uri template from simple encoder") {

    assert(PathEncoder.Segment.string.encodeUriTemplate("name1") === (UriTemplate / PlaceHolder("name1")))
    assert(PathEncoder.Param("pippo").string.encodeUriTemplate("name1") === (UriTemplate && ("pippo", PlaceHolder("name1"))))
    assert(PathEncoder.Fragment.string.encodeUriTemplate("name1") === (UriTemplate &# PlaceHolder("name1")))
    
  }

  test("uri template from path encoder") {
    val enc1 = (Path / "b" / PathEncoder.Segment.string / "a" && PathEncoder.Param("par").int).uriTemplateEncoder

    assert(enc1.encodeUriTemplate("name1", "name2") === (UriTemplate / "b" / PlaceHolder("name1") / "a" && ("par", PlaceHolder("name2"))))

  }
  
  test("uri template encoder contramap") {
    val enc1 = (Path / "b" / PathEncoder.Segment.string / "a" && PathEncoder.Param("par").int).uriTemplateEncoder

    case class Names(n1:String, n2:String)
    
    assert(enc1.contramap(Names.unapply(_:Names).get).encodeUriTemplate(Names("name1", "name2")) === (UriTemplate / "b" / PlaceHolder("name1") / "a" && ("par", PlaceHolder("name2"))))

  }

  test("uri template encoder contramap 1") {
    val enc1 = PathConverter.Segment.string / PathConverter.Segment.string
    
    val enc2 = HTTP("site.com") / enc1.pathConverter 
    val enc3 = HTTP("site.com") / enc1.pathConverter / PathConverter.Segment.string
//   
//    UriTemplate(HTTP("site.com")) / enc1.uriTemplateEncoder
    
    println(PathConverter.Segment.string.uriTemplateEncoder.encodeUriTemplate("par"))
    
    println(enc2.uriTemplateEncoder.encodeUriTemplate("a", "b"))
    println(enc3.uriTemplateEncoder.encodeUriTemplate("a" -> "b", "c"))
  }

}