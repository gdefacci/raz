package org.obl.raz

sealed trait HPath 

object HPath {
  implicit def toHPathOps[H <: HPath](h:H) = new HPathOps[H](h)
  
  implicit def toHPathSegmentAdder[H <: HPath, R <: RelativePathAspect, T](path:HPathCons[H,R,CanAddPath, CanHavePathAsPrefix,T]) =
    new HPathSegmentAdder[H, R, T](path)
    
  implicit def toHParamAdder[H <: HPath, R <: RelativePathAspect, P <: CanHavePrefixAspect,T](path:HPathCons[H,R,CanAddParam,P,T]) =
    new HParamAdder[H, R, P,T](path:HPathCons[H,R,CanAddParam,P,T]) 
   
}

case class HPathNil[+R <: RelativePathAspect,+A <: CanAddAspect,+P <: CanHavePrefixAspect](path:BasePath[R,A,P]) extends HPath {
  override def toString = s"HPathNil($path)"
}

class HPathConsFactory[+R <: RelativePathAspect,+A <: CanAddAspect,+P <: CanHavePrefixAspect] {
  def create[H <: HPath,T](head:H, value:PathF[T]) = HPathCons[H,R,A,P,T](head, value)
}

object HPathConsFactory {
  def apply[R <: RelativePathAspect,A <: CanAddAspect,P <: CanHavePrefixAspect]() = new HPathConsFactory[R,A,P]
}

class HPathCons[+H <: HPath,+R <: RelativePathAspect,+A <: CanAddAspect,+P <: CanHavePrefixAspect, T](val head:H, val value:PathF[T]) extends HPath with UnfilteredHPathMatcher[H,R,A,P,T] {
  
  protected override def pathToMatch:HPathCons[H, R, A, P, T] = this
  
  override def toString = {
    s"HPathCons($value, $head)"
  }
}

object HPathCons {
  def apply[H <: HPath,R <: RelativePathAspect,A <: CanAddAspect,P <: CanHavePrefixAspect, T](head:H, value:PathF[T]) = new HPathCons[H,R,A,P,T](head, value)
  
  implicit def toHPathConsOpts[H <: HPath,R <: RelativePathAspect,A <: CanAddAspect,P <: CanHavePrefixAspect, T](value:HPathCons[H,R,A,P,T]) =
    new HPathConsOpts[H,R,A,P, T](value)
    
}


class HPathOps[H <: HPath](value:H) {
  def apply[T,P <: Path](t:T)(implicit hf:HPathF[H, T, P]) = hf(value).apply(t)
  
  def concat[H1 <: HPath, Out <: HPath](h1:H1)(implicit happ:HAppend[H, H1, Out]):Out = happ.concat(value, h1)
  
  def matchPath[T](p:Path)(implicit pathMatcher:PathMatcher[H,T]):Option[PathMatchResult[T, Path]] = pathMatcher.matcher(value).apply(p)
  
  def at[H1 <: HPath](base:String)(implicit atAux:AtAux[H, H1]) = atAux.apply(value)(base)
}

class HPathConsOpts[H <: HPath,R <: RelativePathAspect,A <: CanAddAspect,P <: CanHavePrefixAspect, T](value:HPathCons[H,R,A,P,T]) {
  
  def mapTo[P1, PTH <: Path, TUP](cnv:Converter[TUP,P1])(implicit mapper:HMapper[HPathCons[H,R,A,P,T], TUP], hf:HPathF[HPathCons[H,R,A,P,T], TUP, PTH], pm:PathMatcher[HPathCons[H,R,A,P,T],TUP]) = 
    mapper.create(value, cnv, hf.apply(value), pm.matcher(value) )

  def toUriTemplate[Out <: HPath,TUP,PTH <: Path](t:TUP)(implicit ut:UT[HPathCons[H,R,A,P,T], Out], hf:HPathF[Out, TUP, PTH]):String = {
    val pth = hf ( ut.apply(value) ).apply(t)
    pth.renderUriTemplate
  }
}

object HPathSegmentAdder {
  
  implicit def toHPathCons[H <: HPath, R <: RelativePathAspect, T](p:HPathSegmentAdder[H,R,T]):HPathCons[H,R,CanAddPath, CanHavePathAsPrefix,T] = 
    p.path

  implicit def toHPathConsOps[H <: HPath, R <: RelativePathAspect, T](p:HPathSegmentAdder[H,R,T]) =
    new HPathConsOpts[H,R,CanAddPath,CanHavePathAsPrefix, T](p.path)
    
  implicit def toHPathOps[H <: HPath, R <: RelativePathAspect, T](h:HPathSegmentAdder[H,R,T]) = 
    new HPathOps[HPathCons[H,R,CanAddPath, CanHavePathAsPrefix,T]](h.path)
  
}

object HParamAdder {
  
  implicit def toHPathCons[H <: HPath, R <: RelativePathAspect, P <: CanHavePrefixAspect,T](p:HParamAdder [H,R,P,T]):HPathCons[H,R,CanAddParam,P,T] = 
    p.path

  implicit def toHPathConsOps[H <: HPath, R <: RelativePathAspect, P <: CanHavePrefixAspect,T](p:HParamAdder [H,R,P,T]) =
    new HPathConsOpts[H,R,CanAddParam,P,T](p.path)

  implicit def toHPathOps[H <: HPath, R <: RelativePathAspect, P <: CanHavePrefixAspect,T](p:HParamAdder [H,R,P,T]) = 
    new HPathOps[HPathCons[H,R,CanAddParam,P,T]](p.path)  
    
}


class HPathSegmentAdder[+H <: HPath, +R <: RelativePathAspect, T](val path:HPathCons[H,R,CanAddPath, CanHavePathAsPrefix,T]) extends UnfilteredHPathMatcher[H,R,CanAddPath, CanHavePathAsPrefix,T]  {
  
  protected override def pathToMatch:HPathCons[H,R,CanAddPath, CanHavePathAsPrefix,T] = path
  
  def add(sg:String) = HPathConsFactory[R,CanAddPath, CanHavePathAsPrefix].create(path.head, path.value.addPath(PathSg(Seq(sg))))
  def / (sg:String) = add(sg)
  
  def add[T1](pf: PathSgF[T1]) = HPathConsFactory[R, CanAddPath, CanHavePathAsPrefix].create(path, pf.pathf)
  def /[T1](pf: PathSgF[T1]) = add(pf)
  
  def addParam(nm:String, value:String) = HPathConsFactory[R, CanAddParam, CanHavePathAsPrefix].create(path.head, path.value.addParam(QParamSg(nm, value)))
  def && (nm:String, value:String) = addParam(nm, value)

  def addParam[T1](pf: ParamSgF[T1]) = HPathConsFactory[R, CanAddParam, CanHavePathAsPrefix].create(path, pf.pathf)
  def &&[T1](pf: ParamSgF[T1]) = addParam(pf)
  
  def addFragment(frg:String) = HPathConsFactory[R, CanAddAspect, CanHavePathAsPrefix].create(path.head, path.value.withFragment(frg))
  def ## (str:String) = addFragment(str)
  
  def append[P1 <: CanAddAspect, PA <: CanHavePathAsPrefix](p:BasePath[IsRelativePath, P1, PA]):HPathCons[H,R,P1,CanHavePathAsPrefix,T] = {
    HPathConsFactory[R,P1, CanHavePathAsPrefix].create(path.head, path.value.merge(p))
  }
  def ++ [P1 <: CanAddAspect, PA <: CanHavePathAsPrefix](p:BasePath[IsRelativePath, P1, PA]) = append[P1,PA](p)
  
}

class HParamAdder[+H <: HPath, +R <: RelativePathAspect, P <: CanHavePrefixAspect,T](val path:HPathCons[H,R,CanAddParam,P,T]) extends UnfilteredHPathMatcher[H,R,CanAddParam,P,T] {
  
  protected override def pathToMatch:HPathCons[H,R,CanAddParam,P,T] = path
  
  def addParam(nm:String, value:String) = HPathConsFactory[R,CanAddParam,P].create(path.head, path.value.addParam(QParamSg(nm, value)))
  def && (nm:String, value:String) = addParam(nm, value)
  
  def addParam[T1](pf: ParamSgF[T1]) = HPathConsFactory[R, CanAddParam, P].create(path, pf.pathf)
  def &&[T1](pf: ParamSgF[T1]) = addParam(pf)
  
  def addFragment(frg:String) = HPathConsFactory[R, CanAddAspect,P].create(path.head, path.value.withFragment(frg))
  def ## (str:String) = addFragment(str)
  
  def append[P1 <: CanAddAspect, PA <: CanHaveParamsAsPrefix](p:BasePath[IsRelativePath, P1, PA]) = {
    HPathConsFactory[R, P1, P].create(path.head, path.value.merge(p))
  }
  def ++ [P1 <: CanAddAspect, PA <: CanHaveParamsAsPrefix](p:BasePath[IsRelativePath, P1, PA]) = append[P1,PA](p)
  
}
