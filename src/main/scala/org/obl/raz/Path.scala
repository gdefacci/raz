package org.obl.raz

import unfiltered.request._
import unfiltered.request.{ Path => UPath, Params => UParams }

object Path {
  val empty = Raz

  def apply(path: PathSg, params: Seq[QParamSg]) = RootUri(path, params)

  def apply(pathsg: PathSg): RootPath = RootPath(pathsg)
  def apply(qsg: QParamSg): RootParams = RootParams(Seq(qsg))

  def unapply(p: Path): Option[(PathSg, Seq[QParamSg])] = {
    Some(p.path -> p.params)
  }
}

trait Path {
  def path: PathSg
  def params: Seq[QParamSg]

  override def hashCode = {
    path.hashCode + params.hashCode * 11
  }

  override def equals(a: Any): Boolean = a match {
    case p: Path if (p.path == path && p.params == params) => true
    case _ => false
  }

  private[raz] lazy val unprefixed = {
    val pars =
      if (params.isEmpty) ""
      else (Seq(params.head.render(true)) ++ params.tail.map(_.render(false))).mkString("")

    path.path.mkString("/") + pars
  }

  lazy val render: String = {
    if (path.isEmpty) unprefixed
    else "/" + unprefixed
  }

  def at(base: String) = new AbsolutePath(base, this)

  def isEmpty = path.isEmpty && params.isEmpty

  override def toString = render

  def unapply[T1](req: HttpRequest[T1]): Option[Path] = {
    FromUnfiltered.toPath(req).flatMap { pth =>
      PathHelper.subtract(pth, Path.this)
    }.flatMap { (rem:Path) =>
      if (rem.isEmpty) Some(this)
      else None
    }
  }
  
  object Partial {
    def unapply[T1](req: HttpRequest[T1]): Option[Path] = {
      FromUnfiltered.toPath(req).flatMap { pth =>
        PathHelper.subtract(pth, Path.this).map( u => pth )
      }
    }
  }
  
//  def param(name: String, v: String):Path
//  def &&(name: String, v: String):Path 
//
//  def param[T1](pf: ParamSgF[T1]):Resource 
//  def &&[T1](pf: ParamSgF[T1]):Resource

}

case class AbsolutePath(base: String, suffix: Path) extends Path {
  def path: PathSg = suffix.path
  def params: Seq[QParamSg] = suffix.params

  override def hashCode = {
    super.hashCode + base.hashCode * 13
  }

  override def equals(a: Any): Boolean = a match {
    case p: AbsolutePath if super.equals(p) => base == p.base
    case _ => false
  }

  override lazy val render: String = {
    val sep = if (base.endsWith("/")) "" else "/"
    base + sep + suffix.unprefixed
  }
}

sealed trait Resource
sealed trait PathResource extends Resource { self =>
//  def addPath(pth:PathSg):PathResource
}

trait HRoot extends Resource with Path

object Resource {
  implicit def toOps[H <: Resource](h: H) = PathOps[H](h)
}

object Raz extends Path {

  def path = PathSg.empty
  def params = Nil

  def path(nm: String): RootPath = new RootPath(path.add(nm))
  def /(nm: String) = path(nm)

  def path[T1](pf: PathSgF[T1]) = new RootPath(PathSg.empty).path(pf)
  def /[T1](pf: PathSgF[T1]) = path(pf)

  def param(name: String, v: String) = new RootParams(Seq(QParamSg(name, v)))
  def &&(name: String, v: String) = param(name, v)

  def param[T1](pf: ParamSgF[T1]) = new RootParams(Seq.empty[QParamSg]).param(pf)
  def &&[T1](pf: ParamSgF[T1]) = param(pf)
}

object RootPath {
  def apply(value: PathSg) = new RootPath(value)
  def unapply(p: RootPath) = Some(p.value)
}

class RootPath(val value: PathSg) extends Path with PathResource with HRoot {

  def params: Seq[QParamSg] = Nil

  def path = value

  def path(sg: String) = RootPath(value.add(sg))
  def /(nm: String) = path(nm)

  def path[T1](pf: PathSgF[T1]) = new PathHResource(this, pf.pathf)
  def /[T1](pf: PathSgF[T1]) = path(pf)

  def param(nm: String, v: String): RootUri = new RootUri(path, Seq(QParamSg(nm, v)))
  def &&(name: String, v: String) = param(name, v)

  def param[T1](pf: ParamSgF[T1]) = new ParamsHResource(this, pf.pathf)
  def &&[T1](pf: ParamSgF[T1]) = param(pf)
  
  def addPath(pth:PathSg) = new RootPath(value.add(pth))
}

class BaseRootPath(val prefix: Seq[String]) extends RootPath(PathSg(prefix))

object RootUri {
  def apply(value: PathSg, params: Seq[QParamSg]) = new RootUri(value, params)
  def unapply(p: RootUri) = Some(p.value -> p.params)
}

class RootUri(val value: PathSg, val params: Seq[QParamSg]) extends Path with Resource with HRoot {

  def path = value

  def param(nm: String, v: String) = RootUri(value, params = params ++ Seq(QParamSg(nm, v)))
  def &&(name: String, v: String) = param(name, v)

  def param[T1](pf: ParamSgF[T1]) = new ParamsHResource(this, pf.pathf)
  def &&[T1](pf: ParamSgF[T1]) = param(pf)

}

object RootParams {
  def apply(params: Seq[QParamSg]) = new RootParams(params)
  def unapply(p: RootParams) = Some(p.params)
}

class RootParams(val params: Seq[QParamSg]) extends Path with Resource with HRoot {

  val path = PathSg.empty

  def param(nm: String, v: String) = RootParams(params = params ++ Seq(QParamSg(nm, v)))
  def &&(name: String, v: String) = param(name, v)

  def param[T1](pf: ParamSgF[T1]) = new ParamsHResource(this, pf.pathf)
  def &&[T1](pf: ParamSgF[T1]) = param(pf)

}

sealed trait HResource[+H <: Resource, +T] extends Resource {
  def head: H
  def value: PathF[T]

  def unapply[T1, H1 >: HResource[H, T], TR](req: HttpRequest[T1])(implicit pm: PathMatcher[H1, TR]): Option[TR] = {
    FromUnfiltered.toPath(req).flatMap { pth =>
      pm.matcher(this)(pth) match {
        case Some(PathMatchResult(v, rest)) if rest.isEmpty => Some(v)
        case _ => None
      }
    }
  }

  object Partial {
    def unapply[T1, H1 >: HResource[H, T], TR](req: HttpRequest[T1])(implicit pm: PathMatcher[H1, TR]): Option[TR] = {
      FromUnfiltered.toPath(req).flatMap { pth =>
        pm.matcher(HResource.this)(pth) match {
          case Some(PathMatchResult(v, rest)) => Some(v)
          case _ => None
        }
      }
    }
  }
}

final class PathHResource[+H <: PathResource, +T] private[raz] (val head: H, val value: PathF[T]) extends HResource[H, T] with PathResource {
  def path(sg: String) = new PathHResource[H, T](head, value.addPath(PathSg(Seq(sg))))
  def /(nm: String) = path(nm)

  def path[T1](pf: PathSgF[T1]) = new PathHResource(this, pf.pathf)
  def /[T1](pf: PathSgF[T1]) = path(pf)

  def param(name: String, v: String) = {
    new PathAndParamsHResource[H, T](head, value.addParam(QParamSg(name, v)))
  }

  def &&(name: String, v: String) = param(name, v)

  def param[T1](pf: ParamSgF[T1]) = new ParamsHResource(this, pf.pathf)
  def &&[T1](pf: ParamSgF[T1]) = param(pf)
  
  def addPath(pth:PathSg) = new PathHResource[H,T](head, value.addPath(pth))

  override def toString = "PathHResource(" + head + ", " + value + ")"

}

final class PathAndParamsHResource[+H <: PathResource, +T] private[raz] (val head: H, val value: PathF[T]) extends HResource[H, T] {
  def param(name: String, v: String) = new PathAndParamsHResource(head, value.addParam(QParamSg(name, v)))
  def &&(name: String, v: String) = param(name, v)

  def param[T1](pf: ParamSgF[T1]) = new ParamsHResource(this, pf.pathf)
  def &&[T1](pf: ParamSgF[T1]) = param(pf)

  override def toString = "PathAndParamsHResource(" + head + ", " + value + ")"

}

final class ParamsHResource[+H <: Resource, +T] private[raz] (val head: H, val value: PathF[T]) extends HResource[H, T] {

  def param(name: String, v: String) = new ParamsHResource(head, value.addParam(QParamSg(name, v)))
  def &&(name: String, v: String) = param(name, v)

  def param[T1](pf: ParamSgF[T1]) = new ParamsHResource(this, pf.pathf)
  def &&[T1](pf: ParamSgF[T1]) = param(pf)

  override def toString = "ParamsHResource(" + head + ", " + value + ")"
}