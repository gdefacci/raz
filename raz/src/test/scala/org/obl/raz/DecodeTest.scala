package org.obl.raz

import org.junit._
import org.junit.Assert._
import PathConverter._

import scalaz.{-\/, \/, \/-}

class DecodeTest {

  
  @Test
  def testDecode1 = {
  
    val r1 = RelativePath / Segment.string / Segment.string
    
    r1.decodeFull(RelativePath / "a" / "b") match {
      case \/-(r) => {
         assertEquals(r._1, "a")
         assertEquals(r._2, "b")
      }
      case -\/(e) => throw e
    } 
  }
  
  @Test
  def testDecode2 = {
  
    val r1 = RelativePath / Segment.string / Segment.string && Param.string("aa")
    
    r1.decodeFull(RelativePath / "a" / "b" && ("aa", "beee")) match {
      case \/-(r) => {
         assertEquals(r._1, "a")
         assertEquals(r._2, "b")
         assertEquals(r._3, "beee")
      }
      case -\/(e) => throw e
    } 
  }
  
  
}