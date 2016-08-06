package org.obl.raz

import org.scalatest.FunSuite
import org.obl.raz.UriTemplate.PlaceHolder
import org.obl.raz.UriTemplate.PlaceHolder

class UriTemplateEncoderTest extends FunSuite {

  import UriTemplateEncoder._

  import UriTemplate.{ PlaceHolder }

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