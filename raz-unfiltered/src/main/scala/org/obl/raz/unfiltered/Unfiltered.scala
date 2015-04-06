package org.obl.raz.unfiltered

import org.obl.raz
import unfiltered.request._
import scalaz.{-\/, \/, \/-}
import org.obl.raz.PathDecoder
import org.obl.raz.PathPosition
import org.obl.raz.PathConverter
import org.obl.raz.ext.PathConversion

import scala.language.implicitConversions

object Unfiltered  {
  
  implicit def unfilteredExtPathDecode[REQ] = new raz.ext.ExtPathDecode[HttpRequest[REQ]] {
    def apply(i:HttpRequest[REQ]):raz.Path = fromUnfiltered(i).get
  }
  
  private [unfiltered] def fromUnfiltered[T1](req: HttpRequest[T1]): Option[raz.Path] = {
    req match {
      case Path(Seg(elems)) & Params(pars) => {
        val params: Seq[raz.QParamSg] = pars.flatMap { e =>
          e._2.map(v => raz.QParamSg(e._1, Some(v)))
        }.toSeq
        val psgs = raz.PathSg(elems.map(java.net.URLDecoder.decode(_, "UTF-8")))
        Some(raz.BasePath(None, psgs, params, None))
      }
      case x => None
    }
  }
  
}