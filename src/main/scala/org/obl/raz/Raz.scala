package org.obl.raz

trait RootHPathFactory[R <: RelativePathAspect] {
  
  def base:Option[String]
  
  lazy val pathRoot = BasePath[R, CanAddPath, CanHavePathAsPrefix](base,PathSg.empty, Seq.empty, None)
  lazy val paramRoot = BasePath[R, CanAddParam, CanHaveParamsAsPrefix](base,PathSg.empty, Seq.empty, None)
  
  def add(sg:String) = BasePath[R,CanAddPath, CanHavePathAsPrefix](base, PathSg(Seq(sg)), Seq.empty, None)
  def / (sg:String) = add(sg)
 
  def add[T1](pf: PathSgF[T1]) = HPathConsFactory[R, CanAddPath, CanHavePathAsPrefix].create(HPathNil(pathRoot), pf.pathf)
  def /[T1](pf: PathSgF[T1]) = add(pf)
  
  def addParam(nm:String, value:String) = BasePath[R, CanAddParam, CanHaveParamsAsPrefix](base, PathSg.empty, Seq(QParamSg(nm, value)), None)
  def && (nm:String, value:String) = addParam(nm, value)
  
  def addParam[T1](pf: ParamSgF[T1]) = HPathConsFactory[R, CanAddParam, CanHaveParamsAsPrefix].create(
      HPathNil(paramRoot), pf.pathf)
      
  def &&[T1](pf: ParamSgF[T1]) = addParam(pf)
  
  def fragment(frg:String) = BasePath[R, CanAddAspect, CanHaveParamsAsPrefix](base, PathSg.empty, Seq.empty, Some(frg))
  def ## (str:String) = fragment(str)
 
}

object Raz extends RootHPathFactory[IsRelativePath] {
  
  lazy val base = None

  def Absolute(basePath:String) = new RootHPathFactory[IsAbsolutePath] {
    def base = Some(basePath)
  }
  
}

object OldRaz {
  
  lazy val empty = BasePath[IsRelativePath, CanAddPath, CanHavePathAsPrefix](None,PathSg.empty, Seq.empty, None)
  
  def add(sg:String) = BasePath[IsRelativePath,CanAddPath, CanHavePathAsPrefix](None, PathSg(Seq(sg)), Seq.empty, None)
  def / (sg:String) = add(sg)
 
  def add[T1](pf: PathSgF[T1]) = HPathConsFactory[IsRelativePath, CanAddPath, CanHavePathAsPrefix].create(HPathNil(empty), pf.pathf)
  def /[T1](pf: PathSgF[T1]) = add(pf)
  
  def addParam(nm:String, value:String) = BasePath[IsRelativePath, CanAddParam, CanHaveParamsAsPrefix](None, PathSg.empty, Seq(QParamSg(nm, value)), None)
  def && (nm:String, value:String) = addParam(nm, value)
  
  def addParam[T1](pf: ParamSgF[T1]) = HPathConsFactory[IsRelativePath, CanAddParam, CanHaveParamsAsPrefix].create(
      HPathNil(BasePath[IsRelativePath, CanAddParam, CanHaveParamsAsPrefix](None,PathSg.empty, Seq.empty, None)), pf.pathf)
      
  def &&[T1](pf: ParamSgF[T1]) = addParam(pf)
  
  def fragment(frg:String) = BasePath[IsRelativePath, CanAddAspect, CanHaveParamsAsPrefix](None, PathSg.empty, Seq.empty, Some(frg))
  def ## (str:String) = fragment(str)
  
}