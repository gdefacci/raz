package org.obl.raz

import org.scalatest.FunSuite
import org.scalatest.Matchers
import scalaz.{ \/- }

class PathBuildersTest extends FunSuite with Matchers {

  test("PathEncoderBuilder ++") {

    import PathEncoder.{ Segment, Param, Fragment }

    val p1 = Segment.string / "segment" / Segment.string
    val p2 = Param("a").string && Param("b").string

    val pr1 = p1 ++ p1
    val pr2 = p1 ++ p2
    val pr3 = p2 ++ p2
    
    assert(pr1.pathEncoder.encode("v1", "v2", "v3", "v4") === (Path / "v1" / "segment" / "v2" / "v3" / "segment" / "v4"))
    assert(pr2.pathEncoder.encode("v1", "v2", "v3", "v4") === (Path / "v1" / "segment" / "v2" && ("a", "v3") && ("b", "v4")))
    assert(pr3.pathEncoder.encode("v1", "v2", "v3", "v4") === (Path && ("a", "v1") && ("b", "v2") && ("a", "v3") && ("b", "v4")))

    "p1 ++ p2" should compile
    "p2 ++ p1" shouldNot compile

    val pr4e:PathEncoder[(String,String,String,String), PathPosition.Segment, PathPosition.Segment] = pr1.pathEncoder
    
    val pr4:PathEncoder[(String,String,String,String), PathPosition.Absolute, PathPosition.Segment]  = (HTTP("mypage.com") / pr4e).pathEncoder
    
    assert( pr4.pathEncoder.encode("v1", "v2", "v3", "v4") === (HTTP("mypage.com") / "v1" / "segment" / "v2" / "v3" / "segment" / "v4") )
  }

  test("PathCodecBuilder ++") {

    import PathCodec.{ Segment, Param, Fragment }

    val p1 = Segment.string / "segment" / Segment.string
    val p2 = Param("a").string && Param("b").string

    val pr1 = p1 ++ p1
    val pr2 = p1 ++ p2
    val pr3 = p2 ++ p2
    
    assert(pr1.pathEncoder.encode("v1", "v2", "v3", "v4") === (Path / "v1" / "segment" / "v2" / "v3" / "segment" / "v4"))
    assert(pr2.pathEncoder.encode("v1", "v2", "v3", "v4") === (Path / "v1" / "segment" / "v2" && ("a", "v3") && ("b", "v4")))
    assert(pr3.pathEncoder.encode("v1", "v2", "v3", "v4") === (Path && ("a", "v1") && ("b", "v2") && ("a", "v3") && ("b", "v4")))

    assert(pr1.pathDecoder.decodeFull(Path / "v1" / "segment" / "v2" / "v3" / "segment" / "v4") == \/-("v1", "v2", "v3", "v4"))
    assert(pr2.pathDecoder.decodeFull(Path / "v1" / "segment" / "v2" && ("a", "v3") && ("b", "v4")) == \/-("v1", "v2", "v3", "v4"))
    assert(pr3.pathDecoder.decodeFull(Path && ("a", "v1") && ("b", "v2") && ("a", "v3") && ("b", "v4")) == \/-("v1", "v2", "v3", "v4"))

    "p1 ++ p2" should compile
    "p2 ++ p1" shouldNot compile

  }

  test("PathConverterBuilder ++") {

    import PathConverter.{ Segment, Param, Fragment }
    import UriTemplate.PlaceHolder

    val p1 = Segment.string / "segment" / Segment.string
    val p2 = Param("a").string && Param("b").string

    val pr1 = p1 ++ p1
    val pr2 = p1 ++ p2
    val pr3 = p2 ++ p2

    assert(pr1.pathEncoder.encode("v1", "v2", "v3", "v4") === (Path / "v1" / "segment" / "v2" / "v3" / "segment" / "v4"))
    assert(pr2.pathEncoder.encode("v1", "v2", "v3", "v4") === (Path / "v1" / "segment" / "v2" && ("a", "v3") && ("b", "v4")))
    assert(pr3.pathEncoder.encode("v1", "v2", "v3", "v4") === (Path && ("a", "v1") && ("b", "v2") && ("a", "v3") && ("b", "v4")))

    assert(pr1.pathDecoder.decodeFull(Path / "v1" / "segment" / "v2" / "v3" / "segment" / "v4") == \/-("v1", "v2", "v3", "v4"))
    assert(pr2.pathDecoder.decodeFull(Path / "v1" / "segment" / "v2" && ("a", "v3") && ("b", "v4")) == \/-("v1", "v2", "v3", "v4"))
    assert(pr3.pathDecoder.decodeFull(Path && ("a", "v1") && ("b", "v2") && ("a", "v3") && ("b", "v4")) == \/-("v1", "v2", "v3", "v4"))

    assert(pr1.uriTemplateEncoder.encodeUriTemplate("v1", "v2", "v3", "v4") === (UriTemplate / PlaceHolder("v1") / "segment" / PlaceHolder("v2") / PlaceHolder("v3") / "segment" / PlaceHolder("v4")))
    assert(pr2.uriTemplateEncoder.encodeUriTemplate("v1", "v2", "v3", "v4") === (UriTemplate / PlaceHolder("v1") / "segment" / PlaceHolder("v2") && ("a", PlaceHolder("v3")) && ("b", PlaceHolder("v4"))))
    assert(pr3.uriTemplateEncoder.encodeUriTemplate("v1", "v2", "v3", "v4") === (UriTemplate && ("a", PlaceHolder("v1")) && ("b", PlaceHolder("v2")) && ("a", PlaceHolder("v3")) && ("b", PlaceHolder("v4"))))

    "p1 ++ p2" should compile
    "p2 ++ p1" shouldNot compile

  }

  test("PathDecoderBuilder ++") {

    import PathDecoder.{ Segment, Param, Fragment }

    val p1 = Segment.string / "segment" / Segment.string
    val p2 = Param("a").string && Param("b").string

    val pr1 = p1 ++ p1
    val pr2 = p1 ++ p2
    val pr3 = p2 ++ p2

    assert(pr1.pathDecoder.decodeFull(Path / "v1" / "segment" / "v2" / "v3" / "segment" / "v4") == \/-("v1", "v2", "v3", "v4"))
    assert(pr2.pathDecoder.decodeFull(Path / "v1" / "segment" / "v2" && ("a", "v3") && ("b", "v4")) == \/-("v1", "v2", "v3", "v4"))
    assert(pr3.pathDecoder.decodeFull(Path && ("a", "v1") && ("b", "v2") && ("a", "v3") && ("b", "v4")) == \/-("v1", "v2", "v3", "v4"))

    "p1 ++ p2" should compile
    "p2 ++ p1" shouldNot compile

  }

  test("PathEncoderBuilder forbidden patterns") {

    import PathEncoder.{ Segment, Param, Fragment }

    """Segment.string / Param("p").int""" shouldNot compile

    """Param("p").int / Segment.string """ shouldNot compile
    """Param("p").int && Segment.string """ shouldNot compile
    """Fragment.int / Segment.string """ shouldNot compile
    """Fragment.int && Segment.string """ shouldNot compile
    """Fragment.int && Param("p").int """ shouldNot compile
    """Param("p").int &# Fragment.int """ should compile
  }

  test("PathDecoderBuilder forbidden patterns") {

    import PathDecoder.{ Segment, Param, Fragment }

    """Segment.string / Param("p").int""" shouldNot compile

    """Param("p").int / Segment.string """ shouldNot compile
    """Param("p").int && Segment.string """ shouldNot compile
    """Fragment.int / Segment.string """ shouldNot compile
    """Fragment.int && Segment.string """ shouldNot compile
    """Fragment.int && Param("p").int """ shouldNot compile
    """Param("p").int &# Fragment.int """ should compile
  }

  test("PathCodecBuilder forbidden patterns") {

    import PathCodec.{ Segment, Param, Fragment }

    """Segment.string / Param("p").int""" shouldNot compile

    """Param("p").int / Segment.string """ shouldNot compile
    """Param("p").int && Segment.string """ shouldNot compile
    """Fragment.int / Segment.string """ shouldNot compile
    """Fragment.int && Segment.string """ shouldNot compile
    """Fragment.int && Param("p").int """ shouldNot compile
    """Param("p").int &# Fragment.int """ should compile
  }

  test("PathConverterBuilder forbidden patterns") {

    import PathConverter.{ Segment, Param, Fragment }

    """Segment.string / Param("p").int""" shouldNot compile

    """Param("p").int / Segment.string """ shouldNot compile
    """Param("p").int && Segment.string """ shouldNot compile
    """Fragment.int / Segment.string """ shouldNot compile
    """Fragment.int && Segment.string """ shouldNot compile
    """Fragment.int && Param("p").int """ shouldNot compile
    """Param("p").int &# Fragment.int """ should compile
  }

}