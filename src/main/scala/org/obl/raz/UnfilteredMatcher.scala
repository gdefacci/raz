package org.obl.raz

import unfiltered.request._
import unfiltered.request.{ Path => UPath, Params => UParams }

object FromUnfiltered {

  def toPath[T1](req: HttpRequest[T1]): Option[Path] = {
    req match {
      case UPath(Seg(elems)) & UParams(pars) => {
        val params: Seq[QParamSg] = pars.flatMap { e =>
          e._2.map(v => QParamSg(e._1, Some(v)))
        }.toSeq
        val psgs = PathSg(elems.map(java.net.URLDecoder.decode(_, "UTF-8")))
        Some(Path(None, psgs, params, None))
      }
      case x => None
    }
  }

  def pathExtract(pathToMatch: Path, req: HttpRequest[_]): Option[Path] = {
    toPath(req).flatMap { pth =>
      PathUtils.subtract(pth, pathToMatch).toOption
    }.flatMap { (rem: Path) =>
      if (rem.isEmpty) Some(Path(None, rem.path, rem.params, None))
      else None
    }
  }

}

trait UnfilteredMatcher { self: Path =>

  def unapply[T1](req: HttpRequest[T1]): Option[Path] = {
    FromUnfiltered.pathExtract(self, req)
  }

  object Partial {
    def unapply[T1](req: HttpRequest[T1]): Option[Path] = {
      FromUnfiltered.toPath(req).flatMap { pth =>
        PathUtils.subtract(pth, self).toOption
      }
    }
  }

}

trait UnfilteredHPathMatcher[+H <: HPath, +R <: RelativePathAspect, A <: CanAddAspect, +P <: CanHavePrefixAspect, TD, TE, UT] {

  protected def pathToMatch: HPathCons[H, R, A, P, TD, TE, UT]

  def unapply[T1, TR](req: HttpRequest[T1])(implicit pm: PathMatcher[HPathCons[H, R, A, P, TD, TE, UT], TR]): Option[TR] = {
    FromUnfiltered.toPath(req).flatMap { pth =>
      pm.decoder(pathToMatch).decodeFull(pth).toOption
    }
  }

  object Partial {
    def unapply[T1, TR](req: HttpRequest[T1])(implicit pm: PathMatcher[HPathCons[H, R, A, P, TD, TE, UT], TR]): Option[TR] = {
      FromUnfiltered.toPath(req).flatMap { pth =>
        pm.decoder(pathToMatch).decode(pth).fold(
          err => None,
          v => Some(v.value))
      }
    }
  }

}