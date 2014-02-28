package org.obl.raz

trait Path extends UnfilteredMatcher {
  
  def base:Option[String]
  def path:PathSg
  def params:Seq[QParamSg]
  def fragment:Option[String]

  def isEmpty = base.isEmpty && path.isEmpty && params.isEmpty && fragment.isEmpty
  
  override def hashCode = base.hashCode + path.hashCode * 11 + params.hashCode * 17 + fragment.hashCode * 23
  
  override def equals(o:Any):Boolean = o match {
    case othr:Path => base == othr.base && path == othr.path && params == othr.params && fragment == othr.fragment
    case x => false
  }
  
  def render:String = {
    val sb = new StringBuilder
    base.foreach{ bs =>
      if (bs.endsWith("/")) sb.append(sb.substring(0, bs.length-1))
      else sb.append(bs) 
    }
    
    val pars =
      if (params.isEmpty) ""
      else (Seq(params.head.render(true)) ++ params.tail.map(_.render(false))).mkString("")

    if (!path.isEmpty) {  
      sb.append("/")
      sb.append(path.path.mkString("/"))
      sb.append(pars)
    } else {
      sb.append(pars)
    }
    
    fragment.foreach{ frg =>
      sb.append("#"+frg)
    }
    
    sb.toString
  }
  
  override def toString = render
}

object Path  {
  lazy val empty = apply(None, PathSg.empty, Nil, None)
  
  def apply(base:Option[String], path:PathSg, params:Seq[QParamSg], fragment:Option[String]) = BasePath[RelativePathAspect, CanAddAspect, CanHavePrefixAspect](base,path,params,fragment)
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

class BasePath[+R <: RelativePathAspect,+A <: CanAddAspect,+P <: CanHavePrefixAspect](val base:Option[String], val path:PathSg, val params:Seq[QParamSg], val fragment:Option[String]) extends Path

object BasePath {
  def empty[R <: RelativePathAspect,A <: CanAddAspect, P <: CanHavePrefixAspect]:BasePath[R,A,P] = apply[R,A,P](None, PathSg.empty, Nil, None)
  
  def apply[R <: RelativePathAspect,A <: CanAddAspect, P <: CanHavePrefixAspect](base:Option[String], path:PathSg, params:Seq[QParamSg], fragment:Option[String]) = 
    new BasePath[R,A,P](base, path, params, fragment)
  
  implicit def toPathSegmentAdder[R <: RelativePathAspect](path:BasePath[R,CanAddPath,CanHavePathAsPrefix])  = new PathSegmentAdder[R](path)
  implicit def toParamAdder[R <: RelativePathAspect,P <: CanHavePrefixAspect](path:BasePath[R,CanAddParam,P]) = new ParamAdder[R,P](path)
  implicit def toAbsolutePathFactory[A <: CanAddAspect,P <: CanHavePrefixAspect](path:BasePath[IsRelativePath,A,P]) = new AbsolutePathFactory[A](path) 
}

class PathSegmentAdder[+R <: RelativePathAspect](path:BasePath[R,CanAddPath, CanHavePathAsPrefix])  {
  
  def add(sg:String) = BasePath[R,CanAddPath, CanHavePathAsPrefix](path.base, path.path.add(sg), path.params, path.fragment)
  def / (sg:String) = add(sg)
  
  def add[T1](pf: PathSgF[T1]) = HPathConsFactory[R, CanAddPath, CanHavePathAsPrefix].create(HPathNil(path), pf.pathf)
  def /[T1](pf: PathSgF[T1]) = add(pf)
  
  def addParam(nm:String, value:String) = BasePath[R, CanAddParam, CanHavePathAsPrefix](path.base, path.path, (path.params ++ Seq(QParamSg(nm, value))), path.fragment)
  def && (nm:String, value:String) = addParam(nm, value)

  def addParam[T1](pf: ParamSgF[T1]) = HPathConsFactory[R, CanAddParam, CanHaveParamsAsPrefix].create(HPathNil(path), pf.pathf)
  def &&[T1](pf: ParamSgF[T1]) = addParam(pf)
  
  def addFragment(frg:String) = BasePath[R, CanAddAspect, CanHavePathAsPrefix](path.base, path.path, path.params, Some(frg))
  def ## (str:String) = addFragment(str)
  
  def append[P1 <: CanAddAspect, PA <: CanHavePathAsPrefix](p:BasePath[IsRelativePath, P1, PA]) =
    BasePath[R, P1, CanHavePathAsPrefix](path.base, PathSg(path.path.path ++ p.path.path), path.params ++ p.params, p.fragment)
    
  def ++ [P1 <: CanAddAspect, PA <: CanHavePathAsPrefix](p:BasePath[IsRelativePath, P1, PA]) = append[P1,PA](p)
  
  def append[H <: HPath, Out <: HPath](p:H)(implicit happ:HAppend[HPathNil[R,CanAddPath, CanHavePathAsPrefix], H, Out]) =
    happ.concat(HPathNil(path), p)
  
  def ++ [H <: HPath, Out <: HPath](p:H)(implicit happ:HAppend[HPathNil[R,CanAddPath, CanHavePathAsPrefix], H, Out]) = append[H,Out](p)  
    
}

class ParamAdder[+R <: RelativePathAspect, P <: CanHavePrefixAspect](path:BasePath[R,CanAddParam,P])  {
  
  def addParam(nm:String, value:String) = BasePath[R,CanAddParam,P](path.base, path.path, path.params ++ Seq(QParamSg(nm, value)), path.fragment)
  def && (nm:String, value:String) = addParam(nm, value)
  
  def addParam[T1](pf: ParamSgF[T1]) = HPathConsFactory[R, CanAddParam, P].create(HPathNil(path), pf.pathf)
  def &&[T1](pf: ParamSgF[T1]) = addParam(pf)
  
  def addFragment(frg:String) = BasePath[R, CanAddAspect,P](path.base, path.path, path.params, Some(frg))
  def ## (str:String) = addFragment(str)
  
  def append[P1 <: CanAddAspect, PA <: CanHaveParamsAsPrefix](p:BasePath[IsRelativePath, P1, PA]) =
    BasePath[R, P1, P](path.base, PathSg(path.path.path ++ p.path.path), path.params ++ p.params, p.fragment)
    
  def ++ [P1 <: CanAddAspect, PA <: CanHaveParamsAsPrefix](p:BasePath[IsRelativePath, P1, PA]) = append[P1,PA](p)
  
  def append[H <: HPath, Out <: HPath](p:H)(implicit happ:HAppend[HPathNil[R,CanAddParam, P], H, Out]) =
    happ.concat(HPathNil(path), p)
  
  def ++ [H <: HPath, Out <: HPath](p:H)(implicit happ:HAppend[HPathNil[R,CanAddParam, P], H, Out]) = append[H,Out](p)
}

class AbsolutePathFactory[A <: CanAddAspect](path:BasePath[IsRelativePath,A,_]) {
  
  def at(base:String) = BasePath[IsAbsolutePath,A,CanHavePrefixAspect](Some(base), path.path, path.params, path.fragment)
  
}

