package org.obl.raz.gen

object FMapperFactoryGen {

  
  def genFMaperFactory(n:Int) = {
  val tpars = 1.to(n).map(i => "T"+i).mkString(",")
    val typ = if (n == 1) tpars else "("+tpars+")"
    
    
    s"""
  implicit def fmapperFactory${n}a[$tpars] = new FMapperFactory[HParamsElems.RootPath.ParamsHResource$n[$tpars], $typ, FMapper.TPathAndPar] {
    def apply(h:HParamsElems.RootPath.ParamsHResource$n[$tpars]) = FMapper.fmapper${n}a[$tpars](h)
  }
  
  implicit def fmapperFactory${n}b[$tpars] = new FMapperFactory[HParamsElems.RootUri.ParamsHResource$n[$tpars], $typ, FMapper.TPar[RootUri]#Type] {
    def apply(h:HParamsElems.RootUri.ParamsHResource$n[$tpars]) = FMapper.fmapper${n}b[$tpars](h)
  }
  
  implicit def fmapperFactory${n}c[$tpars] = new FMapperFactory[HParamsElems.RootParams.ParamsHResource$n[$tpars], $typ, FMapper.TPar[RootParams]#Type] {
    def apply(h:HParamsElems.RootParams.ParamsHResource$n[$tpars]) = FMapper.fmapper${n}c[$tpars](h)
  }
  
  implicit def fmapperFactory${n}d[$tpars] = new FMapperFactory[HPathElems.PathHResource$n[$tpars], $typ, FMapper.TPath] {
    def apply(h:HPathElems.PathHResource$n[$tpars]) = FMapper.fmapper${n}d[$tpars](h)
  }
  
  implicit def fmapperFactory${n}e[$tpars] = new FMapperFactory[HPathAndParamsElems.PathAndParamsHResource$n[$tpars], $typ, FMapper.TPathAndPar] {
    def apply(h:HPathAndParamsElems.PathAndParamsHResource$n[$tpars]) = FMapper.fmapper${n}e[$tpars](h)
  }"""
  }
  
  def main(args:Array[String]):Unit = {
    val max = 17
    
    println( 1.to(max).map(genFMaperFactory).mkString("\n") )
  }
  
}