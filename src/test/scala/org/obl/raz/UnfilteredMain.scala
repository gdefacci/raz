package org.obl.raz

object UnfilteredMain {

   def testUnfiltered = {
      import unfiltered.filter._
      import unfiltered.request._
      import unfiltered.response._
      import unfiltered.request.{ Path => UPath, Params => UParams }
  
      import unfiltered.filter.Intent
  
      val Pth = Raz / ""
      val Pth1 = Raz / "p1" / pathVar[String] / pathVar[Int]
  
      Intent {
        case GET(Pth(x)) => Ok
        case GET(Pth1((str,i))) => {
          val ls:String = str
          val li:Int = i
          Ok
        }
      }
    }

}