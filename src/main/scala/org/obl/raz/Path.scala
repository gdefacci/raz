package org.obl.raz

trait Protocol {
  def render:String
  
  def apply(host:String, port:Int = 80) = PathBase(this, host, port)
}
case object HTTP extends Protocol {
  val render = "http"
}
case object HTTPS extends Protocol {
  val render = "https"
}

case class PathBase(protocol:Protocol, host:String, port:Int) {
  def render = {
    val prfx = s"${protocol.render}://$host"
    if (port == 80) prfx
    else s"$prfx:$port"
  }
}


trait Path extends UnfilteredMatcher with PathRenderer {
  
  def base:Option[PathBase]
  def path:PathSg
  def params:Seq[QParamSg]
  def fragment:Option[String]

  def isEmpty = base.isEmpty && path.isEmpty && params.isEmpty && fragment.isEmpty
  
  override def hashCode = base.hashCode + path.hashCode * 11 + params.hashCode * 17 + fragment.hashCode * 23
  
  override def equals(o:Any):Boolean = o match {
    case othr:Path => base == othr.base && path == othr.path && params.toSet == othr.params.toSet && fragment == othr.fragment
    case x => false
  }
  
  override final def toString = render

  def copy(base: Option[PathBase] = this.base,
    path: PathSg = this.path,
    params: Seq[QParamSg] = this.params,
    fragment: Option[String] = this.fragment) = Path(base, path, params, fragment)
}

object Path  {
  lazy val empty = apply(None, PathSg.empty, Nil, None)
  
  def apply(base:Option[PathBase], path:PathSg, params:Seq[QParamSg], fragment:Option[String]) = BasePath[RelativePathAspect, CanAddAspect, CanHavePrefixAspect](base,path,params,fragment)
  def apply(path:PathSg, params:Seq[QParamSg]) = BasePath[IsRelativePath, CanAddParam, CanHavePathAsPrefix](None,path,params,None)
  def apply(path:PathSg) = BasePath[IsRelativePath, CanAddPath, CanHavePathAsPrefix](None,path,Nil,None)
  def apply(params:Seq[QParamSg]) = BasePath[IsRelativePath, CanAddParam, CanHaveParamsAsPrefix](None,PathSg.empty,params,None)
  
  def unapply(p:Path) = Some(p.base, p.path, p.params, p.fragment)
}

sealed trait RelativePathAspect

abstract class IsRelativePath extends RelativePathAspect
abstract class IsAbsolutePath extends RelativePathAspect

sealed trait CanAddAspect
sealed trait NonFragmentPath extends CanAddAspect

abstract class CanAddPath extends NonFragmentPath
abstract class CanAddParam extends NonFragmentPath

sealed trait CanHavePrefixAspect
abstract class CanHavePathAsPrefix extends CanHavePrefixAspect
abstract class CanHaveParamsAsPrefix extends CanHavePathAsPrefix

class BasePath[+R <: RelativePathAspect,+A <: CanAddAspect,+P <: CanHavePrefixAspect](val base:Option[PathBase], val path:PathSg, val params:Seq[QParamSg], val fragment:Option[String]) extends Path 
//{
//  override def toString = render
//}

object BasePath {
  def empty[R <: RelativePathAspect,A <: CanAddAspect, P <: CanHavePrefixAspect]:BasePath[R,A,P] = apply[R,A,P](None, PathSg.empty, Nil, None)
  
  def apply[R <: RelativePathAspect,A <: CanAddAspect, P <: CanHavePrefixAspect](base:Option[PathBase], path:PathSg, params:Seq[QParamSg], fragment:Option[String]) = 
    new BasePath[R,A,P](base, path, params, fragment)
  
  implicit def toPathSegmentAdder[R <: RelativePathAspect](path:BasePath[R,CanAddPath,CanHavePathAsPrefix])  = new PathSegmentAdder[R](path)
  implicit def toParamAdder[R <: RelativePathAspect, A <: NonFragmentPath, P <: CanHavePrefixAspect](path:BasePath[R,A,P]) = new ParamAdder[R,A,P](path)
  implicit def toFragmentAdder[R <: RelativePathAspect,A <: NonFragmentPath, P <: CanHavePrefixAspect](path:BasePath[R,NonFragmentPath,P]) = new FragmentAdder[R,A,P](path)
  implicit def toAbsolutePathFactory[A <: CanAddAspect,P <: CanHavePrefixAspect](path:BasePath[IsRelativePath,A,P]) = new AbsolutePathFactory[A,P](path)

}

class PathSegmentAdder[+R <: RelativePathAspect](path:BasePath[R,CanAddPath, CanHavePathAsPrefix])  {
  
  def add(sg:String) = BasePath[R,CanAddPath, CanHavePathAsPrefix](path.base, path.path.add(sg), path.params, path.fragment)
  def / (sg:String) = add(sg)
  
  def add[D,E,UT](pf: PathConverter[D,E,UT,CanAddPath,CanHavePathAsPrefix]) = HPathConsFactory[R, CanHavePathAsPrefix].create(HPathNil(path), pf)
  def /[D,E,UT](pf: PathConverter[D,E,UT,CanAddPath,CanHavePathAsPrefix])  = add(pf)
  
  def append[P1 <: CanAddAspect, PA <: CanHavePathAsPrefix](p:BasePath[IsRelativePath, P1, PA]) =
    BasePath[R, P1, CanHavePathAsPrefix](path.base, PathSg(path.path.path ++ p.path.path), path.params ++ p.params, p.fragment)
    
  def ++ [P1 <: CanAddAspect, PA <: CanHavePathAsPrefix](p:BasePath[IsRelativePath, P1, PA]) = append[P1,PA](p)
  
  def append[H <: HPath, Out <: HPath](p:H)(implicit happ:HAppend[HPathNil[R,CanAddPath, CanHavePathAsPrefix], H, Out]) =
    happ.concat(HPathNil(path), p)
  
  def ++ [H <: HPath, Out <: HPath](p:H)(implicit happ:HAppend[HPathNil[R,CanAddPath, CanHavePathAsPrefix], H, Out]) = append[H,Out](p)  
    
}

class ParamAdder[+R <: RelativePathAspect, A <: NonFragmentPath, P <: CanHavePrefixAspect](path:BasePath[R,A,P])  {
  
  def addParam(nm:String, value:String) = BasePath[R,A,P](path.base, path.path, path.params ++ Seq(QParamSg(nm, value)), path.fragment)
  def && (nm:String, value:String) = addParam(nm, value)
  
  def addParam[D,E,UT](pf: PathConverter[D,E,UT,CanAddParam, CanHaveParamsAsPrefix]) = HPathConsFactory[R,P].create(HPathNil(path), pf)
  def &&[D,E,UT](pf: PathConverter[D,E,UT,CanAddParam, CanHaveParamsAsPrefix])  = addParam(pf)
  
  def append[P1 <: CanAddAspect, PA <: CanHaveParamsAsPrefix](p:BasePath[IsRelativePath, P1, PA]) =
    BasePath[R, P1, P](path.base, PathSg(path.path.path ++ p.path.path), path.params ++ p.params, p.fragment)
    
  def ++ [P1 <: CanAddAspect, PA <: CanHaveParamsAsPrefix](p:BasePath[IsRelativePath, P1, PA]) = append[P1,PA](p)
  
  def append[H <: HPath, Out <: HPath](p:H)(implicit happ:HAppend[HPathNil[R,A, P], H, Out]) =
    happ.concat(HPathNil(path), p)
  
  def ++ [H <: HPath, Out <: HPath](p:H)(implicit happ:HAppend[HPathNil[R,A, P], H, Out]) = append[H,Out](p)
}

class FragmentAdder[+R <: RelativePathAspect, A <: NonFragmentPath, P <: CanHavePrefixAspect](path:BasePath[R,NonFragmentPath,P])  {
  def addFragment(frg:String) = BasePath[R, CanAddAspect,P](path.base, path.path, path.params, Some(frg))
  def ## (str:String) = addFragment(str)
}

class AbsolutePathFactory[A <: CanAddAspect, P <: CanHavePrefixAspect](path:BasePath[IsRelativePath,A,P]) {
  
  def at(base:PathBase) = BasePath[IsAbsolutePath,A,P](Some(base), path.path, path.params, path.fragment)
  
}