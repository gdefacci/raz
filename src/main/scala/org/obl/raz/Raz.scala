package org.obl.raz

trait IRootHPathFactory[R <: RelativePathAspect] {
  type PathBase[+A <: CanAddAspect,+P <: CanHavePrefixAspect]
  type HPathBase[A <: CanAddAspect,+P <: CanHavePrefixAspect,TD1,TE1,UT]
  
  def base:Option[org.obl.raz.PathBase]
  
  def pathRoot[A <: CanAddAspect]:PathBase[A, CanHavePathAsPrefix]
  def paramRoot:PathBase[CanAddParam, CanHaveParamsAsPrefix]
  
  def add(sg:String):PathBase[CanAddPath, CanHavePathAsPrefix]
  def / (sg:String):PathBase[CanAddPath, CanHavePathAsPrefix] = add(sg)
 
  def add[TD1,TE1,A <: CanAddAspect,UT](pf: PathConverter[TD1,TE1,UT,A,CanHavePathAsPrefix]):HPathBase[A, CanHavePathAsPrefix,TD1,TE1,UT]
  def /[TD1,TE1,A <: CanAddAspect,UT](pf: PathConverter[TD1,TE1,UT,A,CanHavePathAsPrefix]):HPathBase[A, CanHavePathAsPrefix,TD1,TE1,UT] = add(pf)
  
  def addParam(nm:String, value:String):PathBase[CanAddParam, CanHaveParamsAsPrefix]
  def && (nm:String, value:String):PathBase[CanAddParam, CanHaveParamsAsPrefix] = addParam(nm, value)
  
  def addParam[TD1,TE1,UT](pf: PathConverter[TD1,TE1,UT,CanAddParam, CanHaveParamsAsPrefix]):HPathBase[CanAddParam, CanHaveParamsAsPrefix,TD1,TE1,UT]
  def &&[TD1,TE1,UT](pf: PathConverter[TD1,TE1,UT,CanAddParam, CanHaveParamsAsPrefix]) = addParam(pf)
  
  def fragment(frg:String):PathBase[CanAddAspect, CanHaveParamsAsPrefix]
  def ## (str:String):PathBase[CanAddAspect, CanHaveParamsAsPrefix] = fragment(str)
 
}

trait RootHPathFactory[R <: RelativePathAspect] extends IRootHPathFactory[R] {
  
  type PathBase[+A <: CanAddAspect,+P <: CanHavePrefixAspect] = BasePath[R,A,P]
  type HPathBase[A <: CanAddAspect,+P <: CanHavePrefixAspect,TD1,TE1,UT] = HPathCons[HPathNil[R,A,P],R,A,P,TD1,TE1,UT]
  
  def pathRoot[A <: CanAddAspect] = BasePath[R, A, CanHavePathAsPrefix](base,PathSg.empty, Seq.empty, None)
  lazy val paramRoot = BasePath[R, CanAddParam, CanHaveParamsAsPrefix](base,PathSg.empty, Seq.empty, None)
  
  def add(sg:String) = BasePath[R,CanAddPath, CanHavePathAsPrefix](base, PathSg(Seq(sg)), Seq.empty, None)
 
  def add[TD1,TE1,A <: CanAddAspect,UT](pf: PathConverter[TD1,TE1,UT,A,CanHavePathAsPrefix]) = HPathConsFactory[R, CanHavePathAsPrefix].create(HPathNil(pathRoot[A]), pf)
  
  def addParam(nm:String, value:String) = BasePath[R, CanAddParam, CanHaveParamsAsPrefix](base, PathSg.empty, Seq(QParamSg(nm, value)), None)
  
  def addParam[TD1,TE1,UT](pf: PathConverter[TD1,TE1,UT,CanAddParam, CanHaveParamsAsPrefix]) = HPathConsFactory[R, CanHaveParamsAsPrefix].create(
      HPathNil(paramRoot), pf)
      
  def fragment(frg:String) = BasePath[R, CanAddAspect, CanHaveParamsAsPrefix](base, PathSg.empty, Seq.empty, Some(frg))
 
}

trait BaseRootHPathFactory[R <: RelativePathAspect] {
  
}

object Raz extends RootHPathFactory[IsRelativePath]  {
  
  lazy val base = None

  def Absolute(basePath:org.obl.raz.PathBase) = new RootHPathFactory[IsAbsolutePath] {
    def base = Some(basePath)
  }
  
}

