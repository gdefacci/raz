package org.obl.raz

import scala.language.higherKinds 
import scala.language.implicitConversions

sealed trait HPath extends ext.ExtUnapply 

object HPath {
  implicit def toHPathOps[H <: HPath](h:H) = new HPathOps[H](h)
  
  implicit def toHPathSegmentAdder[H <: HPath, P >: SegmentPosition <: PathPosition, TD, TE, UT](path:HPathCons[H,P, SegmentPosition,TD, TE,UT]) =
    new HPathSegmentAdder[H,P, TD, TE,UT](path)
    
  implicit def toHParamAdder[H <: HPath, P >: ParamPosition <: PathPosition, S <: P, TD,TE,UT](path:HPathCons[H,P,S,TD, TE,UT]) =
    new HParamAdder[H,P,S,TD,TE,UT](path:HPathCons[H,P,S,TD,TE,UT]) 
   
}

case class HPathNil[P <: PathPosition, S <: P](path:BasePath[P,S]) extends HPath {
  override def toString = s"HPathNil($path)"
}

object HPathNil {
  implicit def apply[P <: PathPosition, S <: P, R[_,_,_]](h:HPathNil[P,S])(implicit pathConversion:ext.PathConversion[R]):R[Path,Path,Path] = {
    pathConversion(h.path)
  }
}

class HPathConsFactory[P <: PathPosition] {
  def create[H <: HPath,TD,TE,UT,S <: P](head:H, value:PathConverter[TD,TE,UT,_,S]):HPathCons[H,P,S,TD,TE,UT] = HPathCons[H,P,S,TD,TE,UT](head, value)
}

object HPathConsFactory {
  def apply[P <: PathPosition]() = new HPathConsFactory[P]
}

class HPathCons[+H <: HPath,+P <: PathPosition, S <: P, TD, TE, UT](val head:H, val value:PathConverter[TD, TE, UT, _, S]) extends HPath /*with UnfilteredHPathMatcher[H,R,A,P,TD,TE,UT] */ {
  
  override def toString = {
    s"HPathCons($value, $head)"
  }
  
}

object HPathCons {
  def apply[H <: HPath,P <: PathPosition, S <: P, TD,TE,UT](head:H, value:PathConverter[TD,TE,UT,_,S]) = new HPathCons[H,P,S,TD,TE,UT](head, value)
}

class HPathOps[H <: HPath](value:H) {
 def apply[T,P <: Path](t:T)(implicit hf:EncHPathF[H, T, P]) = hf(value).apply(t)

 def toUriTemplate[T](t:T)(implicit hf:UTHPathF[H, T]):UriTemplate = hf(value).apply(t)
 
  def concat[H1 <: HPath, Out <: HPath](h1:H1)(implicit happ:HAppend[H, H1, Out]):Out = happ.concat(value, h1)
  
  def at[H1 <: HPath](base:PathBase)(implicit atAux:AtAux[H, H1]) = atAux.apply(value)(base)
}

object HPathSegmentAdder {
  
  implicit def toHPathCons[H <: HPath,P >: SegmentPosition <: PathPosition, TD,TE,UT](p:HPathSegmentAdder[H,P,TD,TE,UT]):HPathCons[H,P, SegmentPosition,TD,TE,UT] = 
    p.path

  implicit def toHPathOps[H <: HPath,P >: SegmentPosition <: PathPosition, TD,TE,UT](h:HPathSegmentAdder[H,P,TD,TE,UT]) = 
    new HPathOps[HPathCons[H,P, SegmentPosition,TD,TE,UT]](h.path)
  
}

object HParamAdder {
  
  implicit def toHPathCons[H <: HPath, P >: ParamPosition <: PathPosition, S <: P, TD,TE,UT](p:HParamAdder[H,P,S,TD,TE,UT]):HPathCons[H,P,S,TD,TE,UT] = 
    p.path

  implicit def toHPathOps[H <: HPath, P >: ParamPosition <: PathPosition, S <: P,TD,TE,UT](p:HParamAdder[H,P,S,TD,TE,UT]) = 
    new HPathOps[HPathCons[H,P,S,TD,TE,UT]](p.path)  
    
}

object HPathBuilder {
  
  implicit def toHPathCons[H <: HPath, P >: SegmentPosition <: PathPosition, S <: P,TD,TE,UT](p:HPathBuilder[H,P,S,TD,TE,UT]):HPathCons[H,P,S,TD,TE,UT] = 
    p.path

  implicit def toHPathOps[H <: HPath, P >: SegmentPosition <: PathPosition, S <: P,TD,TE,UT](p:HPathBuilder[H,P,S,TD,TE,UT]) = 
    new HPathOps[HPathCons[H,P,S,TD,TE,UT]](p.path)  
    
}

trait AbstractHPathSegmentAdder[+H <: HPath, P >: SegmentPosition <: PathPosition, S <: P, TD,TE,UT] {

  def path:HPathCons[H,P,S,TD,TE,UT] 

  def add(sg:String) = HPathConsFactory[P].create[H,TD,TE,UT,SegmentPosition](path.head, path.value.addPart(PathSg(sg)))
  def / (sg:String) = add(sg)
  
  
  def add[D,E,P1 >: SegmentPosition <: S, S1 <: P1,UT](pf: PathConverter[D,E,UT,P1,S1]) = HPathConsFactory[P].create(path, pf)
  
  def /[D,E,P1 >: SegmentPosition <: S, S1 <: P1,UT](pf: PathConverter[D,E,UT,P1,S1]) = add[D,E,P1,S1,UT](pf)
  
}

trait AbstractHParamAdder[+H <: HPath, P >: ParamPosition <: PathPosition, S <: P, TD,TE,UT] {
  
  def path:HPathCons[H,P,S,TD,TE,UT]
  
  def addParam(nm:String, value:String) = HPathConsFactory[P].create[H,TD,TE,UT,ParamPosition](path.head, path.value.addPart(QParamSg(nm, value)))
  def && (nm:String, value:String) = addParam(nm, value)
  
  def addParam[D,E,UT](pf: PathConverter[D,E,UT,ParamPosition, ParamPosition]) = HPathConsFactory[P].create(path, pf)
  def &&[D,E,UT](pf: PathConverter[D,E,UT,ParamPosition, ParamPosition]) = addParam(pf)
  
  def addFragment(frg:String) = HPathConsFactory[P].create(path.head, path.value.addFragmentPart(frg))
  def &# (str:String) = addFragment(str)
  
}

class HPathSegmentAdder[+H <: HPath, P >: SegmentPosition <: PathPosition, TD,TE,UT](val path:HPathCons[H,P, SegmentPosition,TD,TE,UT]) extends 
  AbstractHPathSegmentAdder[H, P, SegmentPosition,TD,TE,UT] { //}with UnfilteredHPathMatcher[H,CanAddPath, CanHavePathAsPrefix,TD,TE,UT] {
  
  def append[P1 <: SegmentPosition, PA <: P1](p:BasePath[P1, PA]):HPathCons[H,SegmentPosition, PA,TD,TE,UT] = {
    HPathConsFactory[SegmentPosition].create[H,TD,TE,UT, PA](path.head, path.value.addPath(p))
  }
  
  def ++ [P1 <: SegmentPosition, PA <: P1](p:BasePath[P1, PA]):HPathCons[H,SegmentPosition, PA,TD,TE,UT] = append[P1,PA](p)
}

class HParamAdder[+H <: HPath, P >: ParamPosition <: PathPosition, S <: P, TD,TE,UT](val path:HPathCons[H,P,S,TD,TE,UT]) extends 
  AbstractHParamAdder[H,P,S,TD,TE,UT] {
  
  def append[P1 <: ParamPosition, PA <: P1](p:BasePath[P1, PA]) = {
    HPathConsFactory[P].create[H,TD,TE,UT, PA](path.head, path.value.addPath[PA](p))
  }
  
  def ++ [P1 <: ParamPosition, PA <: P1](p:BasePath[P1, PA]) = append[P1,PA](p)
} 

class HPathBuilder[+H <: HPath, P >: SegmentPosition  <: PathPosition, S <: P, TD,TE,UT](val path:HPathCons[H,P,S,TD,TE,UT]) extends AbstractHParamAdder[H,P,S,TD,TE,UT] with
  AbstractHPathSegmentAdder[H,P,S,TD,TE,UT] {
  
  def append[P1 <: P, PA <: P1](p:BasePath[P1, PA]):HPathCons[H,P,PA,TD,TE,UT] = {
    HPathConsFactory[P].create(path.head, path.value.addPath(p))
  }
  
  def ++ [P1 <: P, PA <: P1](p:BasePath[P1, PA]) = append[P1,PA](p)
 
}