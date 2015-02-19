package org.obl.raz

import unfiltered.request._
import unfiltered.request.{Path => UPath}
import unfiltered.response._

object TestPar extends Params.Extract("test 2", Params.first)


object EncTestPlan extends unfiltered.filter.Plan {
  def intent = {
    case req @ GET(UPath(Seg("record%202" :: id :: Nil)) & Params(TestPar(v))) => {
      ResponseString(id+"<--->"+v) ~> Ok
    }
  }
}

object EncUnfiltered {
  
  def main(args:Array[String]) = {
	  unfiltered.jetty.Server.local(8080).plan(EncTestPlan).run()
  }

}