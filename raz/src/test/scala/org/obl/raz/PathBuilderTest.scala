package org.obl.raz

import org.scalatest.FunSuite
import org.scalatest.Matchers

class PathBuilderTest extends FunSuite with Matchers{
  
  test("PathBuilder") {
    
    assert( Path / "a" === Path.copy(segments = Seq("a")) )
    assert( (Path && "a") === Path.copy(params = Seq("a" -> None)) )
    assert( (Path && ("a", "b")) === Path.copy(params = Seq("a" -> Some("b")) ))
    assert( (Path &# "a") === Path.copy(fragment = Some("a") ))
    
  }
  
  test("PathBuilder invalid patterns") {
    
    """Path && "a" / "b" """ shouldNot compile
    """Path &# "a" / "b" """ shouldNot compile
    """Path &# "a" && "b" """ shouldNot compile
    """Path / "a" && "b" / "c" """ shouldNot compile
    
    """Path / "a" / PathEncoder.Param("pippo").string """ shouldNot compile 
    """Path / "a" && PathEncoder.Param("pippo").string / "a" """ shouldNot compile
    """Path / "a" && PathEncoder.Param("pippo").string / PathEncoder.Segment.string """ shouldNot compile
    
  }
  
  
  
}