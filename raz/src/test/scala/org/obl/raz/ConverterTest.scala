package org.obl.raz

import scalaz.{ -\/, \/, \/- }
import org.scalatest.Matchers
import org.scalatest.FunSuite

class ConverterTest extends FunSuite with Matchers {

  import PathConverter.{ Segment, Param, Fragment }

  def checkSymmetric[T, UT, S <: PathPosition, E <: PathPosition](converter: PathConverter[T, T, UT, S, E], v: T) = {
    val pth = converter.encode(v)
    converter.decodeFull(pth).fold(
      err => fail(err),
      v1 => assert(v1 === v))
  }

  def checkSymmetric[T, UT, S <: PathPosition, E <: PathPosition](converter: PathConverter[T, T, UT, S, E], pth: Path) = {
    converter.decodeFull(pth).fold(
      err => fail(err),
      { v1 =>
        assert(converter.encode(v1) === pth)
      })
  }

  test("converter segment") {

    checkSymmetric(Segment.string, "segment")
    checkSymmetric(Segment.int, 1)
    checkSymmetric(Segment.boolean, true)

    checkSymmetric(Segment.string, Path / "segment")
    checkSymmetric(Segment.int, Path / "1")
    checkSymmetric(Segment.boolean, Path / "true")

  }

  test("converter param") {

    val par = "par"
    checkSymmetric(Param(par).string, "v1")
    checkSymmetric(Param(par).int, 1)
    checkSymmetric(Param(par).boolean, true)

    checkSymmetric(Param(par).string, Path && (par, "value1"))
    checkSymmetric(Param(par).int, Path && (par, "1"))
    checkSymmetric(Param(par).boolean, Path && (par, "true"))

  }

  test("converter fragment") {

    checkSymmetric(Fragment.string, "v1")
    checkSymmetric(Fragment.int, 1)
    checkSymmetric(Fragment.boolean, true)

    checkSymmetric(Fragment.string, Path &# "value1")
    checkSymmetric(Fragment.int, Path &# "1")
    checkSymmetric(Fragment.boolean, Path &# "true")
  }

  lazy val sampleCodec = {
    HTTP("www.syte.com", 8020) / "a" / Segment.string / "p" && ("par1", "value1") && Param("par2").int &# Fragment.boolean
  }

  test("converter path") {

    val p1 = HTTP("www.syte.com", 8020) / "a" / "segment1" / "p" && ("par1", "value1") && ("par2", "12") &# "false"
    val v1 = ("segment1", 12, false)

    checkSymmetric(sampleCodec.pathConverter, v1)
    checkSymmetric(sampleCodec.pathConverter, p1)

  }

  test("converter case map") {

    case class Cl1(a: String, b: Int, c: Boolean)

    val enc1 = sampleCodec.pathConverter.caseMap(Cl1.tupled)(Cl1.unapply)
    val v = Cl1("segment1", 12, false)
    val pth = HTTP("www.syte.com", 8020) / "a" / "segment1" / "p" && ("par1", "value1") && ("par2", "12") &# "false"

    checkSymmetric(enc1, v)
    checkSymmetric(enc1, pth)

  }

  test("converter case map 1") {

    val cn1 = (Segment.string / Segment.int).pathConverter

    val cn2 = cn1 / Segment.boolean
    val cn2a = Segment.string / Segment.int / Segment.boolean

    case class Cl1(a: (String, Int), c: Boolean)

    val enc2 = cn2.pathConverter.caseMap(Cl1.tupled)(Cl1.unapply)
    val enc2a = cn2a.pathConverter.caseMap(p => Cl1(p._1 -> p._2, p._3))(cl => {
      Cl1.unapply(cl).map(p => (p._1._1, p._1._2, p._2))
    })

    val p1 = Path / "1" / "2" / "true"
    val v1 = Cl1("1" -> 2, true)
    checkSymmetric(enc2, p1)
    checkSymmetric(enc2a, p1)
    checkSymmetric(enc2a, v1)
    checkSymmetric(enc2a, v1)

    val cn3 = Path / "a" / cn1 / Segment.boolean

    val enc3 = cn3.pathConverter.caseMap(Cl1.tupled)(Cl1.unapply)
    val p2 = Path / "a" / "1" / "2" / "true"

    checkSymmetric(enc3, p2)
    checkSymmetric(enc3, v1)

    case class Cl2(a: (String, Int))

    val cn4 = Path / cn1
    val cn4a = HTTP("home.com") / cn1 / Segment.boolean
    val cn4b = HTTP("home.com") / cn1

    val enc4a = cn4a.pathConverter.caseMap(Cl1.tupled)(Cl1.unapply)
    val enc4b = cn4b.pathConverter.caseMap(Cl2.apply(_))(Cl2.unapply)
    val enc4 = cn4.pathConverter.caseMap(Cl2.apply(_))(Cl2.unapply)

    val p4 = Path / "1" / "2"
    val p4a = HTTP("home.com") / "1" / "2" / "true"
    val p4b = HTTP("home.com") / "1" / "2"
    val v4 = Cl2("1" -> 2)

    checkSymmetric(enc4, p4)
    checkSymmetric(enc4a, p4a)
    checkSymmetric(enc4b, p4b)

    checkSymmetric(enc4, v4)
    checkSymmetric(enc4a, v1)
    checkSymmetric(enc4b, v4)

  }

  test("converter append") {

    val c1 = Segment.string.append(Path / "sg2")
    val c2 = Segment.string.append(Path && "sg2")
    val p1 = Path / "segment" / "sg2"
    val p2 = Path / "segment" && "sg2"

    checkSymmetric(c1, "segment")
    checkSymmetric(c2, "segment")

    checkSymmetric(c1, p1)
    checkSymmetric(c2, p2)

    """PathConverter.Segment.string.append(Path && "sg2")""" should compile
    """PathConverter.Param.string.append(Path / "sg2")""" shouldNot compile

    val c3 = Param("par").string.append(Path && "sg2")
    val v3 = "value1"
    val p3 = Path && ("par", "value1") && "sg2"

    checkSymmetric(c3, v3)
    checkSymmetric(c3, p3)

    """PathConverter.Fragment.string.append(Path / "sg2")""" shouldNot compile
    """PathConverter.Fragment.string.append(Path && "sg2")""" shouldNot compile
    """PathConverter.Fragment.string.append(Path &# "sg2")""" shouldNot compile

  }

  test("converter prepend") {

    val c1 = Segment.string.prepend(Path / "sg2")
    val v1 = "segment"
    val p1 = Path / "sg2" / "segment"

    checkSymmetric(c1, v1)
    checkSymmetric(c1, p1)

    //    assert( c1.encode("segment") === (Path / "sg2" / "segment"))

    """PathConverter.Segment.string.prepend(Path && "sg2")""" shouldNot compile
    """PathConverter.Segment.string.prepend(Path &# "sg2")""" shouldNot compile

    val c2 = Param("par").string.prepend(Path / "sg2")
    val v2 = "v1"
    val p2 = Path / "sg2" && ("par", "v1")

    checkSymmetric(c2, v2)
    checkSymmetric(c2, p2)

    //    assert( c2.encode("v1") === ( Path / "sg2" && ("par", "v1")))

    val c3 = Param("par").string.prepend(Path && "sg2")
    val v3 = "value1"
    val p3 = Path && "sg2" && ("par", "value1")

    checkSymmetric(c3, v3)
    checkSymmetric(c3, p3)

    """PathConverter.Param("par").string.prepend(Path &# "sg2")""" shouldNot compile

    val c4 = Fragment.string.prepend(Path / "sg2")
    val c5 = Fragment.string.prepend(Path && "sg2")
    val p4 = Path / "sg2" &# "v1"
    val p5 = Path && "sg2" &# "v1"

    checkSymmetric(c4, v2)
    checkSymmetric(c5, v2)

    checkSymmetric(c4, p4)
    checkSymmetric(c5, p5)

    """(PathConverter.Fragment.string.prepend(Path &# "sg2")).encode("v1")""" shouldNot compile

  }

  test("decoderAt and uriTemplateEncoderAt") {

	  val pth1 = HTTP("www.syte.com", 8020) / "a" / "1" / "p" && ("par1", "value1") &# "false"
	  val pth2 = Path / "1" / "p" && ("par1", "value1") &# "false"
    
	  val pc1 = (HTTP("www.syte.com", 8020) / "a" / Segment.string / "p" && ("par1", "value1") &# "false").pathConverter

    val pc2 = pc1.decoderAt(HTTP("www.syte.com", 8020) / "a").uriTemplateEncoderAt(HTTP("www.syte.com", 8020))

    assert( pc1.decodeFull(pth1) == \/-("1") )
    assert( pc2.encode("1") == pth1 )
    assert( pc2.encodeUriTemplate("name").render == "/a/{name}/p?par1=value1#false" )
    assert( pc2.fullPath.decodeFull(pth1) == \/-("1") )
    
  }

}