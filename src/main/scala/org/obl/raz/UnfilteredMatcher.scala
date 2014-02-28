package org.obl.raz

import unfiltered.request._
import unfiltered.request.{ Path => UPath, Params => UParams }

private[raz] object FromUnfiltered {

  def toPath[T1](req: HttpRequest[T1]): Option[Path] = {
    req match {
      case UPath(Seg(elems)) & UParams(pars) => {
        val params: Seq[QParamSg] = pars.flatMap { e =>
          e._2.map(v => QParamSg(e._1, Some(v)))
        }.toSeq
        val psgs = PathSg(elems)
        Some(Path(None, psgs, params, None))
      }
      case x => None
    }
  }

}

trait UnfilteredMatcher { self: Path =>

  def unapply[T1](req: HttpRequest[T1]): Option[Path] = {
    FromUnfiltered.toPath(req).flatMap { pth =>
      PathHelper.subtract(pth, self)
    }.flatMap { (rem: Path) =>
      if (rem.isEmpty) Some(Path(None, rem.path, rem.params, None))
      else None
    }
  }

  object Partial {
    def unapply[T1](req: HttpRequest[T1]): Option[Path] = {
      FromUnfiltered.toPath(req).flatMap { pth =>
        PathHelper.subtract(pth, self)
      }
    }
  }

}

trait UnfilteredHPathMatcher[+H <: HPath, +R <: RelativePathAspect, +A <: CanAddAspect, +P <: CanHavePrefixAspect, T] { 
  //self: HPathCons[H, R, A, P, T] =>

  protected def pathToMatch:HPathCons[H, R, A, P, T]
  
  def unapply[T1, TR](req: HttpRequest[T1])(implicit pm: PathMatcher[HPathCons[H, R, A, P, T], TR]): Option[TR] = {
    FromUnfiltered.toPath(req).flatMap { pth =>
      pm.matcher(pathToMatch)(pth) match {
        case Some(PathMatchResult(v, rest)) if rest.isEmpty => Some(v)
        case _ => None
      }
    }
  }

  object Partial {
    def unapply[T1, TR](req: HttpRequest[T1])(implicit pm: PathMatcher[HPathCons[H, R, A, P, T], TR]): Option[TR] = {
      FromUnfiltered.toPath(req).flatMap { pth =>
        pm.matcher(pathToMatch)(pth) match {
          case Some(PathMatchResult(v, rest)) => Some(v)
          case _ => None
        }
      }
    }
  }

}
