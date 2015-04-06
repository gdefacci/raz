package org.obl.raz

import org.junit.Assert
import scalaz.{-\/, \/-}

object TestHelper {

  import EncHPathF._
  
  def check[P <: PathPosition, S <: P, T](h:PathConverter[T,T,_,P,S], v:T) = {
    h.decodeFull(h.encode(v)) match {
      case \/-(v1) => Assert.assertEquals(v,v1)
      case x => Assert.fail(x.toString)
    }
  }

  
}