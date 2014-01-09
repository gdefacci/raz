package org.obl.raz.gen

object FMapperGen {
  
  def genMatcher(n:Int):String = {
    
    val tpars = 1.to(n).map(i => "T"+i).mkString(",")
    val typ = if (n == 1) tpars else "("+tpars+")"
    
  s"""
def matcher$n[$tpars, P](h: HResource$n[$tpars], cnv: ($typ) => P): Path => Option[PathMatchResult[P, Path]] = { pth =>
  BasePathMatchers.matcher$n[$tpars](h)(pth).map(r => r.mapValue(cnv))
}"""
  }
  
  
  def genPathFunction(n:Int) = {
	val tpars = 1.to(n).map(i => "T"+i).mkString(",")
    val typ = if (n == 1) tpars else "("+tpars+")"
    
    s"""
def pathFunction$n[$tpars, P](h: HResource$n[$tpars], cnv: P => Option[$typ]): P => Path = { p =>
  cnv(p).map(pathFSum$n(h)).getOrElse(emptyPath)
}"""
  }
  
  def genPathF(n:Int) = {
	val tpars = 1.to(n).map(i => "T"+i).mkString(",")
    val typ = if (n == 1) tpars else "("+tpars+")"
    
    s"""
def pathf$n[$tpars,P](h:HResource$n[$tpars], cnv: Converter[$typ,P]) = {
  PathF(pathFunction$n(h, cnv.unapply), matcher$n(h, cnv.apply _), h.value.expansionKind)
}"""
  }
  
  def genFMappers(n:Int) = {
    val tpars = 1.to(n).map(i => "T"+i).mkString(",")
    val typ = if (n == 1) tpars else "("+tpars+")"
    val heads = "h."+(1.to(n).map(i=>"head").mkString("."))
    
    s"""
implicit def fmapper${n}a[$tpars](h:HParamsElems.RootPath.ParamsHResource$n[$tpars]) = new FMapper[HParamsElems.RootPath.ParamsHResource$n[$tpars], $typ, TPathAndPar] {
  def mapTo[P](cnv:Converter[$typ,P]):PathAndParamsHResource[RootPath, P] = new PathAndParamsHResource[RootPath,P]($heads, pathf$n(h, cnv))
}
  
implicit def fmapper${n}b[$tpars](h:HParamsElems.RootUri.ParamsHResource$n[$tpars]) = new FMapper[HParamsElems.RootUri.ParamsHResource$n[$tpars], $typ, TPar[RootUri]#Type] {
  def mapTo[P](cnv:Converter[$typ,P]):ParamsHResource[RootUri, P] = new ParamsHResource[RootUri,P]($heads, pathf$n(h, cnv))
}
  
implicit def fmapper${n}c[$tpars](h:HParamsElems.RootParams.ParamsHResource$n[$tpars]) = new FMapper[HParamsElems.RootParams.ParamsHResource$n[$tpars], $typ, TPar[RootParams]#Type] {
  def mapTo[P](cnv:Converter[$typ,P]):ParamsHResource[RootParams, P] = new ParamsHResource[RootParams,P]($heads, pathf$n(h, cnv))
}
  
implicit def fmapper${n}d[$tpars](h:HPathElems.PathHResource$n[$tpars]) = new FMapper[HPathElems.PathHResource$n[$tpars], $typ, TPath] {
  def mapTo[P](cnv:Converter[$typ,P]):PathHResource[RootPath, P] = new PathHResource[RootPath,P]($heads, pathf$n(h, cnv))
}
  
implicit def fmapper${n}e[$tpars](h:HPathAndParamsElems.PathAndParamsHResource$n[$tpars]) = new FMapper[HPathAndParamsElems.PathAndParamsHResource$n[$tpars], $typ, TPathAndPar] {
  def mapTo[P](cnv:Converter[$typ,P]):PathAndParamsHResource[RootPath, P] = new PathAndParamsHResource[RootPath,P]($heads, pathf$n(h, cnv))
}"""
  }
  
  def main(args:Array[String]):Unit = {
    val max = 17
    
    println( 1.to(max).map(genMatcher).mkString("\n") )
    println( 1.to(max).map(genPathFunction).mkString("\n") )
    println( 1.to(max).map(genPathF).mkString("\n") )
    println( 1.to(max).map(genFMappers).mkString("\n") )
  }

}