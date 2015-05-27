package org.obl.raz

import org.junit._
import org.junit.Assert._

import scalaz.\/-

class ChooseTest {

  import PathConverter._
  
  sealed trait Sg
  case class StringSg(value:String) extends Sg
  case class IntSg(value:Int) extends Sg
  
  val sgEncoder:PathEncoder[Sg] = PathEncoder.stringSegment.contramap { sg:Sg =>
    sg match {
      case StringSg(str) => str
      case IntSg(i) => i.toString
    }
  }
  
  val sgDecoder:PathDecoder[Sg] = PathDecoder.intSegment.map(IntSg).orElse( PathDecoder.stringSegment.map(StringSg) )
  
  val sg = Segment.factory(sgDecoder, sgEncoder, UriTemplateEncoder.Simple.segment)
  
  
  @Test
  def test1 = {
  
    val u1 = RelativePath / "abba" / seq(sg) &&  ("yeah", "ok")
    
    val m = Seq[Sg](StringSg("ba"), IntSg(12), StringSg("ab"))
    
    u1.decode(RelativePath / "abba" / "ba" / "12" / "ab" / "11" / "yeah" &&  ("yeah", "ok")) match {
      case \/-(pmr) => {
        assertTrue(pmr.rest.isEmpty)
        assertEquals(pmr.value, Seq(StringSg("ba"), IntSg(12), StringSg("ab"), IntSg(11), StringSg("yeah")))
      }
      case x => ???
    }
    
    assertEquals(RelativePath / "abba" / "ba" / "12" / "ab" &&  ("yeah", "ok"), u1(m))
    
  }
}