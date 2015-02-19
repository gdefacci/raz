package org.obl.raz

sealed trait HPath 

object HPath {
  implicit def toHPathOps[H <: HPath](h:H) = new HPathOps[H](h)
  
  implicit def toHPathSegmentAdder[H <: HPath, R <: RelativePathAspect, TD, TE, UT](path:HPathCons[H,R,CanAddPath, CanHavePathAsPrefix,TD, TE,UT]) =
    new HPathSegmentAdder[H, R, TD, TE,UT](path)
    
  implicit def toHParamAdder[H <: HPath, R <: RelativePathAspect, A <: NonFragmentPath, P <: CanHavePrefixAspect,TD, TE,UT](path:HPathCons[H,R,A,P,TD, TE,UT]) =
    new HParamAdder[H, R, A, P,TD, TE, UT](path:HPathCons[H,R,A,P,TD,TE,UT]) 
   
}


case class HPathNil[+R <: RelativePathAspect,+A <: CanAddAspect,+P <: CanHavePrefixAspect](path:BasePath[R,A,P]) extends HPath {
  override def toString = s"HPathNil($path)"
}

class HPathConsFactory[+R <: RelativePathAspect, P <: CanHavePrefixAspect] {
  def create[H <: HPath,TD,TE,UT,A <: CanAddAspect](head:H, value:PathConverter[TD,TE,UT,A,_]) = HPathCons[H,R,A,P,TD,TE,UT](head, value)
}

object HPathConsFactory {
  def apply[R <: RelativePathAspect, P <: CanHavePrefixAspect]() = new HPathConsFactory[R,P]
}

class HPathCons[+H <: HPath,+R <: RelativePathAspect,A <: CanAddAspect,+P <: CanHavePrefixAspect, TD, TE, UT](val head:H, val value:PathConverter[TD, TE, UT, A,_]) extends HPath with UnfilteredHPathMatcher[H,R,A,P,TD,TE,UT] {
  
  protected override def pathToMatch:HPathCons[H,R,A,P,TD,TE,UT] = this
  
  override def toString = {
    s"HPathCons($value, $head)"
  }
}

object HPathCons {
  def apply[H <: HPath,R <: RelativePathAspect,A <: CanAddAspect,P <: CanHavePrefixAspect, TD,TE,UT](head:H, value:PathConverter[TD,TE,UT,A,_]) = new HPathCons[H,R,A,P,TD,TE,UT](head, value)

//  implicit def toHPathConsOpts[H <: HPath,R <: RelativePathAspect,A <: CanAddAspect,P <: CanHavePrefixAspect, TD, TE, UT](value:HPathCons[H,R,A,P,TD,TE,UT]) =
//    new HPathConsOpts[H,R,A,P,TD,TE,UT](value)

}



class HPathOps[H <: HPath](value:H) {
 def apply[T,P <: Path](t:T)(implicit hf:EncHPathF[H, T, P]) = hf(value).apply(t)

 def toUriTemplate[T](t:T)(implicit hf:UTHPathF[H, T]):UriTemplate = hf(value).apply(t)
 
  def concat[H1 <: HPath, Out <: HPath](h1:H1)(implicit happ:HAppend[H, H1, Out]):Out = happ.concat(value, h1)
  
  def at[H1 <: HPath](base:PathBase)(implicit atAux:AtAux[H, H1]) = atAux.apply(value)(base)
}

//class HPathConsOpts[H <: HPath,R <: RelativePathAspect,A <: CanAddAspect,P <: CanHavePrefixAspect, TD, TE, UT](value:HPathCons[H,R,A,P,TD,TE,UT]) {
//}

  
object HPathSegmentAdder {
  
  implicit def toHPathCons[H <: HPath, R <: RelativePathAspect, TD,TE,UT](p:HPathSegmentAdder[H,R,TD,TE,UT]):HPathCons[H,R,CanAddPath, CanHavePathAsPrefix,TD,TE,UT] = 
    p.path

//  implicit def toHPathConsOps[H <: HPath, R <: RelativePathAspect, TD,TE,UT](p:HPathSegmentAdder[H,R,TD,TE,UT]) =
//    new HPathConsOpts[H,R,CanAddPath,CanHavePathAsPrefix, TD,TE,UT](p.path)
    
  implicit def toHPathOps[H <: HPath, R <: RelativePathAspect, TD,TE,UT](h:HPathSegmentAdder[H,R,TD,TE,UT]) = 
    new HPathOps[HPathCons[H,R,CanAddPath, CanHavePathAsPrefix,TD,TE,UT]](h.path)
  
}

object HParamAdder {
  
  implicit def toHPathCons[H <: HPath, R <: RelativePathAspect, A <: NonFragmentPath, P <: CanHavePrefixAspect,TD,TE,UT](p:HParamAdder[H,R,A,P,TD,TE,UT]):HPathCons[H,R,A,P,TD,TE,UT] = 
    p.path

//  implicit def toHPathConsOps[H <: HPath, R <: RelativePathAspect, A <: NonFragmentPath, P <: CanHavePrefixAspect,TD,TE,UT](p:HParamAdder[H,R,A,P,TD,TE,UT]) =
//    new HPathConsOpts[H,R,A,P,TD,TE,UT](p.path)

  implicit def toHPathOps[H <: HPath, R <: RelativePathAspect, A <: NonFragmentPath, P <: CanHavePrefixAspect,TD,TE,UT](p:HParamAdder[H,R,A,P,TD,TE,UT]) = 
    new HPathOps[HPathCons[H,R,A,P,TD,TE,UT]](p.path)  
    
}

object HPathBuilder {
  
  implicit def toHPathCons[H <: HPath, R <: RelativePathAspect, A <: NonFragmentPath, P <: CanHavePrefixAspect,TD,TE,UT](p:HPathBuilder[H,R,A,P,TD,TE,UT]):HPathCons[H,R,A,P,TD,TE,UT] = 
    p.path

//  implicit def toHPathConsOps[H <: HPath, R <: RelativePathAspect, A <: NonFragmentPath, P <: CanHavePrefixAspect,TD,TE,UT](p:HPathBuilder[H,R,A,P,TD,TE,UT]) =
//    new HPathConsOpts[H,R,A,P,TD,TE,UT](p.path)

  implicit def toHPathOps[H <: HPath, R <: RelativePathAspect, A <: NonFragmentPath, P <: CanHavePrefixAspect,TD,TE,UT](p:HPathBuilder[H,R,A,P,TD,TE,UT]) = 
    new HPathOps[HPathCons[H,R,A,P,TD,TE,UT]](p.path)  
    
}

trait AbstractHPathSegmentAdder[+H <: HPath, +R <: RelativePathAspect, A <: CanAddAspect, P <: CanHavePrefixAspect, TD,TE,UT] {

  def path:HPathCons[H,R,A,P,TD,TE,UT] 

  def add(sg:String) = HPathConsFactory[R,P].create(path.head, path.value.addPart(PathSg(sg)))
  def / (sg:String) = add(sg)
  
  
  def add[D,E,A <: CanAddAspect,UT](pf: PathConverter[D,E,UT,A,CanHavePathAsPrefix]) = HPathConsFactory[R,P].create(path, pf)
  def /[D,E,A <: CanAddAspect,UT](pf: PathConverter[D,E,UT,A,CanHavePathAsPrefix]) = add(pf)
  
}

class HPathSegmentAdder[+H <: HPath, +R <: RelativePathAspect, TD,TE,UT](val path:HPathCons[H,R,CanAddPath, CanHavePathAsPrefix,TD,TE,UT]) extends 
  AbstractHPathSegmentAdder[H,R, CanAddPath, CanHavePathAsPrefix,TD,TE,UT] with UnfilteredHPathMatcher[H,R,CanAddPath, CanHavePathAsPrefix,TD,TE,UT] {
  
  protected override def pathToMatch:HPathCons[H,R,CanAddPath, CanHavePathAsPrefix,TD,TE,UT] = path

  def append[P1 <: CanAddAspect, PA <: CanHavePathAsPrefix](p:BasePath[IsRelativePath, P1, PA]):HPathCons[H,R,P1,CanHavePathAsPrefix,TD,TE,UT] = {
    HPathConsFactory[R, CanHavePathAsPrefix].create(path.head, path.value.addPath[P1](p))
  }
  
  def ++ [P1 <: CanAddAspect, PA <: CanHavePathAsPrefix](p:BasePath[IsRelativePath, P1, PA]) = append[P1,PA](p)
}

trait AbstractHParamAdder[+H <: HPath, +R <: RelativePathAspect, A <: NonFragmentPath, P <: CanHavePrefixAspect,TD,TE,UT] {
  
  def path:HPathCons[H,R,A,P,TD,TE,UT]
  
  def addParam(nm:String, value:String) = HPathConsFactory[R,P].create(path.head, path.value.addPart(QParamSg(nm, value)))
  def && (nm:String, value:String) = addParam(nm, value)
  
  def addParam[D,E,UT](pf: PathConverter[D,E,UT,CanAddParam, CanHaveParamsAsPrefix]) = HPathConsFactory[R,P].create(path, pf)
  def &&[D,E,UT](pf: PathConverter[D,E,UT,CanAddParam, CanHaveParamsAsPrefix]) = addParam(pf)
  
  def addFragment(frg:String) = HPathConsFactory[R,P].create(path.head, path.value.addFragmentPart(frg))
  def ## (str:String) = addFragment(str)
  
}

class HParamAdder[+H <: HPath, +R <: RelativePathAspect, A <: NonFragmentPath, P <: CanHavePrefixAspect,TD,TE,UT](val path:HPathCons[H,R,A,P,TD,TE,UT]) extends 
  AbstractHParamAdder[H,R,A,P,TD,TE,UT] with UnfilteredHPathMatcher[H,R,A,P,TD,TE,UT]{
  protected override def pathToMatch:HPathCons[H,R,A,P,TD,TE,UT] = path
  
  def append[P1 <: CanAddAspect, PA <: CanHaveParamsAsPrefix](p:BasePath[IsRelativePath, P1, PA]) = {
    HPathConsFactory[R,P].create(path.head, path.value.addPath[P1](p))
  }
  
  def ++ [P1 <: CanAddAspect, PA <: CanHaveParamsAsPrefix](p:BasePath[IsRelativePath, P1, PA]) = append[P1,PA](p)
} 

class HPathBuilder[+H <: HPath, +R <: RelativePathAspect, A <: NonFragmentPath, P <: CanHavePrefixAspect, TD,TE,UT](val path:HPathCons[H,R,A,P,TD,TE,UT]) extends AbstractHParamAdder[H,R,A,P,TD,TE,UT] with
  AbstractHPathSegmentAdder[H,R,A,P,TD,TE,UT] with UnfilteredHPathMatcher[H,R,A,P,TD,TE,UT] {
  protected override def pathToMatch:HPathCons[H,R,A,P,TD,TE,UT] = path
  
  def append[P1 <: CanAddAspect, PA <: P](p:BasePath[IsRelativePath, P1, PA]):HPathCons[H,R,P1,P,TD,TE,UT] = {
    HPathConsFactory[R,P].create(path.head, path.value.addPath[P1](p))
  }
  
  def ++ [P1 <: CanAddAspect, PA <: P](p:BasePath[IsRelativePath, P1, PA]) = append[P1,PA](p)
 
}