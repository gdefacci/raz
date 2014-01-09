package org.obl.raz

import unfiltered.request._
import unfiltered.request.{Path => UPath, Params => UParams}

private[raz] object FromUnfiltered {

  def toPath[T1](req: HttpRequest[T1]): Option[Path] = {
    req match {
      case UPath(Seg(elems)) & UParams(pars) => {
        val params: Seq[QParamSg] = pars.flatMap { e =>
          e._2.map(v => QParamSg(e._1, Some(v)))
        }.toSeq
        val psgs = PathSg(elems)
        Some(org.obl.raz.Path(psgs, params))
      }
      case x => None
    }
  }
  
}