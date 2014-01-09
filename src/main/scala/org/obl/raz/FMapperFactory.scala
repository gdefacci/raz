package org.obl.raz

trait FMapperFactory[-H, T, +HR[_]] {
  def apply(h:H):FMapper[H, T, HR]
}

object FMapperFactory {

  
//  implicit def fmapperFactory2a[T1,T2] = new FMapperFactory[HParamsElems.RootPath.HParamsElem2[T1, T2], (T1,T2), FMapper.TPathAndPar] {
//    def apply(h:ParamsHResource[HResource[RootPath,T1], T2]) = FMapper.fmapper2a[T1,T2](h)
//  }
//  
//  implicit def fmapperFactory2b[T1,T2] = new FMapperFactory[HParamsElems.RootUri.HParamsElem2[T1, T2], (T1,T2), FMapper.TPar[RootUri]#Type] {
//    def apply(h:ParamsHResource[HResource[RootUri,T1], T2]) = FMapper.fmapper2b[T1,T2](h)
//  }
//  
//  implicit def fmapperFactory2c[T1,T2] = new FMapperFactory[HParamsElems.RootParams.HParamsElem2[T1, T2], (T1,T2), FMapper.TPar[RootParams]#Type] {
//    def apply(h:ParamsHResource[HResource[RootParams,T1], T2]) = FMapper.fmapper2c[T1,T2](h)
//  }
//  
//  implicit def fmapperFactory2d[T1,T2] = new FMapperFactory[PathHResource[PathHResource[RootPath,T1], T2], (T1,T2), FMapper.TPath] {
//    def apply(h:PathHResource[PathHResource[RootPath,T1], T2]) = FMapper.fmapper2d[T1,T2](h)
//  }
//  
//  implicit def fmapperFactory2e[T1,T2] = new FMapperFactory[PathAndParamsHResource[PathHResource[RootPath,T1], T2], (T1,T2), FMapper.TPathAndPar] {
//    def apply(h:PathAndParamsHResource[PathHResource[RootPath,T1], T2]) = FMapper.fmapper2e[T1,T2](h)
//  }
//
//  // --
//  
//  implicit def fmapperFactory3a[T1,T2,T3] = new FMapperFactory[HParamsElems.RootPath.HParamsElem3[T1,T2,T3], (T1,T2,T3), FMapper.TPathAndPar] {
//    def apply(h:ParamsHResource[HResource[HResource[RootPath,T1],T2], T3]) = FMapper.fmapper3a[T1,T2,T3](h)
//  }
//  
//  implicit def fmapperFactory3b[T1,T2,T3] = new FMapperFactory[ParamsHResource[HResource[HResource[RootUri,T1],T2], T3], (T1,T2,T3), FMapper.TPar[RootUri]#Type] {
//    def apply(h:ParamsHResource[HResource[HResource[RootUri,T1],T2], T3]) = FMapper.fmapper3b[T1,T2,T3](h)
//  }
//  
//  implicit def fmapperFactory3c[T1,T2,T3] = new FMapperFactory[ParamsHResource[HResource[HResource[RootParams,T1],T2],T3], (T1,T2,T3), FMapper.TPar[RootParams]#Type] {
//    def apply(h:ParamsHResource[HResource[HResource[RootParams,T1],T2],T3]) = FMapper.fmapper3c[T1,T2,T3](h)
//  }
//  
//  implicit def fmapperFactory3d[T1,T2,T3] = new FMapperFactory[PathHResource[PathHResource[PathHResource[RootPath,T1],T2],T3], (T1,T2,T3), FMapper.TPath] {
//    def apply(h:PathHResource[PathHResource[PathHResource[RootPath,T1],T2],T3]) = FMapper.fmapper3d[T1,T2,T3](h)
//  }
//  
//  implicit def fmapperFactory3e[T1,T2,T3] = new FMapperFactory[PathAndParamsHResource[PathHResource[PathHResource[RootPath,T1],T2],T3], (T1,T2,T3), FMapper.TPathAndPar] {
//    def apply(h:PathAndParamsHResource[PathHResource[PathHResource[RootPath,T1],T2],T3]) = FMapper.fmapper3e[T1,T2,T3](h)
//  }

  
  implicit def fmapperFactory1a[T1] = new FMapperFactory[HParamsElems.RootPath.HParamsElem1[T1], T1, FMapper.TPathAndPar] {
    def apply(h:HParamsElems.RootPath.HParamsElem1[T1]) = FMapper.fmapper1a[T1](h)
  }
  
  implicit def fmapperFactory1b[T1] = new FMapperFactory[HParamsElems.RootUri.HParamsElem1[T1], T1, FMapper.TPar[RootUri]#Type] {
    def apply(h:HParamsElems.RootUri.HParamsElem1[T1]) = FMapper.fmapper1b[T1](h)
  }
  
  implicit def fmapperFactory1c[T1] = new FMapperFactory[HParamsElems.RootParams.HParamsElem1[T1], T1, FMapper.TPar[RootParams]#Type] {
    def apply(h:HParamsElems.RootParams.HParamsElem1[T1]) = FMapper.fmapper1c[T1](h)
  }
  
  implicit def fmapperFactory1d[T1] = new FMapperFactory[HPathElems.HPathElem1[T1], T1, FMapper.TPath] {
    def apply(h:HPathElems.HPathElem1[T1]) = FMapper.fmapper1d[T1](h)
  }
  
  implicit def fmapperFactory1e[T1] = new FMapperFactory[HPathAndParamsElems.HPathAndParamsElem1[T1], T1, FMapper.TPathAndPar] {
    def apply(h:HPathAndParamsElems.HPathAndParamsElem1[T1]) = FMapper.fmapper1e[T1](h)
  }

  implicit def fmapperFactory2a[T1,T2] = new FMapperFactory[HParamsElems.RootPath.HParamsElem2[T1,T2], (T1,T2), FMapper.TPathAndPar] {
    def apply(h:HParamsElems.RootPath.HParamsElem2[T1,T2]) = FMapper.fmapper2a[T1,T2](h)
  }
  
  implicit def fmapperFactory2b[T1,T2] = new FMapperFactory[HParamsElems.RootUri.HParamsElem2[T1,T2], (T1,T2), FMapper.TPar[RootUri]#Type] {
    def apply(h:HParamsElems.RootUri.HParamsElem2[T1,T2]) = FMapper.fmapper2b[T1,T2](h)
  }
  
  implicit def fmapperFactory2c[T1,T2] = new FMapperFactory[HParamsElems.RootParams.HParamsElem2[T1,T2], (T1,T2), FMapper.TPar[RootParams]#Type] {
    def apply(h:HParamsElems.RootParams.HParamsElem2[T1,T2]) = FMapper.fmapper2c[T1,T2](h)
  }
  
  implicit def fmapperFactory2d[T1,T2] = new FMapperFactory[HPathElems.HPathElem2[T1,T2], (T1,T2), FMapper.TPath] {
    def apply(h:HPathElems.HPathElem2[T1,T2]) = FMapper.fmapper2d[T1,T2](h)
  }
  
  implicit def fmapperFactory2e[T1,T2] = new FMapperFactory[HPathAndParamsElems.HPathAndParamsElem2[T1,T2], (T1,T2), FMapper.TPathAndPar] {
    def apply(h:HPathAndParamsElems.HPathAndParamsElem2[T1,T2]) = FMapper.fmapper2e[T1,T2](h)
  }

  implicit def fmapperFactory3a[T1,T2,T3] = new FMapperFactory[HParamsElems.RootPath.HParamsElem3[T1,T2,T3], (T1,T2,T3), FMapper.TPathAndPar] {
    def apply(h:HParamsElems.RootPath.HParamsElem3[T1,T2,T3]) = FMapper.fmapper3a[T1,T2,T3](h)
  }
  
  implicit def fmapperFactory3b[T1,T2,T3] = new FMapperFactory[HParamsElems.RootUri.HParamsElem3[T1,T2,T3], (T1,T2,T3), FMapper.TPar[RootUri]#Type] {
    def apply(h:HParamsElems.RootUri.HParamsElem3[T1,T2,T3]) = FMapper.fmapper3b[T1,T2,T3](h)
  }
  
  implicit def fmapperFactory3c[T1,T2,T3] = new FMapperFactory[HParamsElems.RootParams.HParamsElem3[T1,T2,T3], (T1,T2,T3), FMapper.TPar[RootParams]#Type] {
    def apply(h:HParamsElems.RootParams.HParamsElem3[T1,T2,T3]) = FMapper.fmapper3c[T1,T2,T3](h)
  }
  
  implicit def fmapperFactory3d[T1,T2,T3] = new FMapperFactory[HPathElems.HPathElem3[T1,T2,T3], (T1,T2,T3), FMapper.TPath] {
    def apply(h:HPathElems.HPathElem3[T1,T2,T3]) = FMapper.fmapper3d[T1,T2,T3](h)
  }
  
  implicit def fmapperFactory3e[T1,T2,T3] = new FMapperFactory[HPathAndParamsElems.HPathAndParamsElem3[T1,T2,T3], (T1,T2,T3), FMapper.TPathAndPar] {
    def apply(h:HPathAndParamsElems.HPathAndParamsElem3[T1,T2,T3]) = FMapper.fmapper3e[T1,T2,T3](h)
  }

  implicit def fmapperFactory4a[T1,T2,T3,T4] = new FMapperFactory[HParamsElems.RootPath.HParamsElem4[T1,T2,T3,T4], (T1,T2,T3,T4), FMapper.TPathAndPar] {
    def apply(h:HParamsElems.RootPath.HParamsElem4[T1,T2,T3,T4]) = FMapper.fmapper4a[T1,T2,T3,T4](h)
  }
  
  implicit def fmapperFactory4b[T1,T2,T3,T4] = new FMapperFactory[HParamsElems.RootUri.HParamsElem4[T1,T2,T3,T4], (T1,T2,T3,T4), FMapper.TPar[RootUri]#Type] {
    def apply(h:HParamsElems.RootUri.HParamsElem4[T1,T2,T3,T4]) = FMapper.fmapper4b[T1,T2,T3,T4](h)
  }
  
  implicit def fmapperFactory4c[T1,T2,T3,T4] = new FMapperFactory[HParamsElems.RootParams.HParamsElem4[T1,T2,T3,T4], (T1,T2,T3,T4), FMapper.TPar[RootParams]#Type] {
    def apply(h:HParamsElems.RootParams.HParamsElem4[T1,T2,T3,T4]) = FMapper.fmapper4c[T1,T2,T3,T4](h)
  }
  
  implicit def fmapperFactory4d[T1,T2,T3,T4] = new FMapperFactory[HPathElems.HPathElem4[T1,T2,T3,T4], (T1,T2,T3,T4), FMapper.TPath] {
    def apply(h:HPathElems.HPathElem4[T1,T2,T3,T4]) = FMapper.fmapper4d[T1,T2,T3,T4](h)
  }
  
  implicit def fmapperFactory4e[T1,T2,T3,T4] = new FMapperFactory[HPathAndParamsElems.HPathAndParamsElem4[T1,T2,T3,T4], (T1,T2,T3,T4), FMapper.TPathAndPar] {
    def apply(h:HPathAndParamsElems.HPathAndParamsElem4[T1,T2,T3,T4]) = FMapper.fmapper4e[T1,T2,T3,T4](h)
  }

  implicit def fmapperFactory5a[T1,T2,T3,T4,T5] = new FMapperFactory[HParamsElems.RootPath.HParamsElem5[T1,T2,T3,T4,T5], (T1,T2,T3,T4,T5), FMapper.TPathAndPar] {
    def apply(h:HParamsElems.RootPath.HParamsElem5[T1,T2,T3,T4,T5]) = FMapper.fmapper5a[T1,T2,T3,T4,T5](h)
  }
  
  implicit def fmapperFactory5b[T1,T2,T3,T4,T5] = new FMapperFactory[HParamsElems.RootUri.HParamsElem5[T1,T2,T3,T4,T5], (T1,T2,T3,T4,T5), FMapper.TPar[RootUri]#Type] {
    def apply(h:HParamsElems.RootUri.HParamsElem5[T1,T2,T3,T4,T5]) = FMapper.fmapper5b[T1,T2,T3,T4,T5](h)
  }
  
  implicit def fmapperFactory5c[T1,T2,T3,T4,T5] = new FMapperFactory[HParamsElems.RootParams.HParamsElem5[T1,T2,T3,T4,T5], (T1,T2,T3,T4,T5), FMapper.TPar[RootParams]#Type] {
    def apply(h:HParamsElems.RootParams.HParamsElem5[T1,T2,T3,T4,T5]) = FMapper.fmapper5c[T1,T2,T3,T4,T5](h)
  }
  
  implicit def fmapperFactory5d[T1,T2,T3,T4,T5] = new FMapperFactory[HPathElems.HPathElem5[T1,T2,T3,T4,T5], (T1,T2,T3,T4,T5), FMapper.TPath] {
    def apply(h:HPathElems.HPathElem5[T1,T2,T3,T4,T5]) = FMapper.fmapper5d[T1,T2,T3,T4,T5](h)
  }
  
  implicit def fmapperFactory5e[T1,T2,T3,T4,T5] = new FMapperFactory[HPathAndParamsElems.HPathAndParamsElem5[T1,T2,T3,T4,T5], (T1,T2,T3,T4,T5), FMapper.TPathAndPar] {
    def apply(h:HPathAndParamsElems.HPathAndParamsElem5[T1,T2,T3,T4,T5]) = FMapper.fmapper5e[T1,T2,T3,T4,T5](h)
  }

  implicit def fmapperFactory6a[T1,T2,T3,T4,T5,T6] = new FMapperFactory[HParamsElems.RootPath.HParamsElem6[T1,T2,T3,T4,T5,T6], (T1,T2,T3,T4,T5,T6), FMapper.TPathAndPar] {
    def apply(h:HParamsElems.RootPath.HParamsElem6[T1,T2,T3,T4,T5,T6]) = FMapper.fmapper6a[T1,T2,T3,T4,T5,T6](h)
  }
  
  implicit def fmapperFactory6b[T1,T2,T3,T4,T5,T6] = new FMapperFactory[HParamsElems.RootUri.HParamsElem6[T1,T2,T3,T4,T5,T6], (T1,T2,T3,T4,T5,T6), FMapper.TPar[RootUri]#Type] {
    def apply(h:HParamsElems.RootUri.HParamsElem6[T1,T2,T3,T4,T5,T6]) = FMapper.fmapper6b[T1,T2,T3,T4,T5,T6](h)
  }
  
  implicit def fmapperFactory6c[T1,T2,T3,T4,T5,T6] = new FMapperFactory[HParamsElems.RootParams.HParamsElem6[T1,T2,T3,T4,T5,T6], (T1,T2,T3,T4,T5,T6), FMapper.TPar[RootParams]#Type] {
    def apply(h:HParamsElems.RootParams.HParamsElem6[T1,T2,T3,T4,T5,T6]) = FMapper.fmapper6c[T1,T2,T3,T4,T5,T6](h)
  }
  
  implicit def fmapperFactory6d[T1,T2,T3,T4,T5,T6] = new FMapperFactory[HPathElems.HPathElem6[T1,T2,T3,T4,T5,T6], (T1,T2,T3,T4,T5,T6), FMapper.TPath] {
    def apply(h:HPathElems.HPathElem6[T1,T2,T3,T4,T5,T6]) = FMapper.fmapper6d[T1,T2,T3,T4,T5,T6](h)
  }
  
  implicit def fmapperFactory6e[T1,T2,T3,T4,T5,T6] = new FMapperFactory[HPathAndParamsElems.HPathAndParamsElem6[T1,T2,T3,T4,T5,T6], (T1,T2,T3,T4,T5,T6), FMapper.TPathAndPar] {
    def apply(h:HPathAndParamsElems.HPathAndParamsElem6[T1,T2,T3,T4,T5,T6]) = FMapper.fmapper6e[T1,T2,T3,T4,T5,T6](h)
  }

  implicit def fmapperFactory7a[T1,T2,T3,T4,T5,T6,T7] = new FMapperFactory[HParamsElems.RootPath.HParamsElem7[T1,T2,T3,T4,T5,T6,T7], (T1,T2,T3,T4,T5,T6,T7), FMapper.TPathAndPar] {
    def apply(h:HParamsElems.RootPath.HParamsElem7[T1,T2,T3,T4,T5,T6,T7]) = FMapper.fmapper7a[T1,T2,T3,T4,T5,T6,T7](h)
  }
  
  implicit def fmapperFactory7b[T1,T2,T3,T4,T5,T6,T7] = new FMapperFactory[HParamsElems.RootUri.HParamsElem7[T1,T2,T3,T4,T5,T6,T7], (T1,T2,T3,T4,T5,T6,T7), FMapper.TPar[RootUri]#Type] {
    def apply(h:HParamsElems.RootUri.HParamsElem7[T1,T2,T3,T4,T5,T6,T7]) = FMapper.fmapper7b[T1,T2,T3,T4,T5,T6,T7](h)
  }
  
  implicit def fmapperFactory7c[T1,T2,T3,T4,T5,T6,T7] = new FMapperFactory[HParamsElems.RootParams.HParamsElem7[T1,T2,T3,T4,T5,T6,T7], (T1,T2,T3,T4,T5,T6,T7), FMapper.TPar[RootParams]#Type] {
    def apply(h:HParamsElems.RootParams.HParamsElem7[T1,T2,T3,T4,T5,T6,T7]) = FMapper.fmapper7c[T1,T2,T3,T4,T5,T6,T7](h)
  }
  
  implicit def fmapperFactory7d[T1,T2,T3,T4,T5,T6,T7] = new FMapperFactory[HPathElems.HPathElem7[T1,T2,T3,T4,T5,T6,T7], (T1,T2,T3,T4,T5,T6,T7), FMapper.TPath] {
    def apply(h:HPathElems.HPathElem7[T1,T2,T3,T4,T5,T6,T7]) = FMapper.fmapper7d[T1,T2,T3,T4,T5,T6,T7](h)
  }
  
  implicit def fmapperFactory7e[T1,T2,T3,T4,T5,T6,T7] = new FMapperFactory[HPathAndParamsElems.HPathAndParamsElem7[T1,T2,T3,T4,T5,T6,T7], (T1,T2,T3,T4,T5,T6,T7), FMapper.TPathAndPar] {
    def apply(h:HPathAndParamsElems.HPathAndParamsElem7[T1,T2,T3,T4,T5,T6,T7]) = FMapper.fmapper7e[T1,T2,T3,T4,T5,T6,T7](h)
  }

  implicit def fmapperFactory8a[T1,T2,T3,T4,T5,T6,T7,T8] = new FMapperFactory[HParamsElems.RootPath.HParamsElem8[T1,T2,T3,T4,T5,T6,T7,T8], (T1,T2,T3,T4,T5,T6,T7,T8), FMapper.TPathAndPar] {
    def apply(h:HParamsElems.RootPath.HParamsElem8[T1,T2,T3,T4,T5,T6,T7,T8]) = FMapper.fmapper8a[T1,T2,T3,T4,T5,T6,T7,T8](h)
  }
  
  implicit def fmapperFactory8b[T1,T2,T3,T4,T5,T6,T7,T8] = new FMapperFactory[HParamsElems.RootUri.HParamsElem8[T1,T2,T3,T4,T5,T6,T7,T8], (T1,T2,T3,T4,T5,T6,T7,T8), FMapper.TPar[RootUri]#Type] {
    def apply(h:HParamsElems.RootUri.HParamsElem8[T1,T2,T3,T4,T5,T6,T7,T8]) = FMapper.fmapper8b[T1,T2,T3,T4,T5,T6,T7,T8](h)
  }
  
  implicit def fmapperFactory8c[T1,T2,T3,T4,T5,T6,T7,T8] = new FMapperFactory[HParamsElems.RootParams.HParamsElem8[T1,T2,T3,T4,T5,T6,T7,T8], (T1,T2,T3,T4,T5,T6,T7,T8), FMapper.TPar[RootParams]#Type] {
    def apply(h:HParamsElems.RootParams.HParamsElem8[T1,T2,T3,T4,T5,T6,T7,T8]) = FMapper.fmapper8c[T1,T2,T3,T4,T5,T6,T7,T8](h)
  }
  
  implicit def fmapperFactory8d[T1,T2,T3,T4,T5,T6,T7,T8] = new FMapperFactory[HPathElems.HPathElem8[T1,T2,T3,T4,T5,T6,T7,T8], (T1,T2,T3,T4,T5,T6,T7,T8), FMapper.TPath] {
    def apply(h:HPathElems.HPathElem8[T1,T2,T3,T4,T5,T6,T7,T8]) = FMapper.fmapper8d[T1,T2,T3,T4,T5,T6,T7,T8](h)
  }
  
  implicit def fmapperFactory8e[T1,T2,T3,T4,T5,T6,T7,T8] = new FMapperFactory[HPathAndParamsElems.HPathAndParamsElem8[T1,T2,T3,T4,T5,T6,T7,T8], (T1,T2,T3,T4,T5,T6,T7,T8), FMapper.TPathAndPar] {
    def apply(h:HPathAndParamsElems.HPathAndParamsElem8[T1,T2,T3,T4,T5,T6,T7,T8]) = FMapper.fmapper8e[T1,T2,T3,T4,T5,T6,T7,T8](h)
  }

  implicit def fmapperFactory9a[T1,T2,T3,T4,T5,T6,T7,T8,T9] = new FMapperFactory[HParamsElems.RootPath.HParamsElem9[T1,T2,T3,T4,T5,T6,T7,T8,T9], (T1,T2,T3,T4,T5,T6,T7,T8,T9), FMapper.TPathAndPar] {
    def apply(h:HParamsElems.RootPath.HParamsElem9[T1,T2,T3,T4,T5,T6,T7,T8,T9]) = FMapper.fmapper9a[T1,T2,T3,T4,T5,T6,T7,T8,T9](h)
  }
  
  implicit def fmapperFactory9b[T1,T2,T3,T4,T5,T6,T7,T8,T9] = new FMapperFactory[HParamsElems.RootUri.HParamsElem9[T1,T2,T3,T4,T5,T6,T7,T8,T9], (T1,T2,T3,T4,T5,T6,T7,T8,T9), FMapper.TPar[RootUri]#Type] {
    def apply(h:HParamsElems.RootUri.HParamsElem9[T1,T2,T3,T4,T5,T6,T7,T8,T9]) = FMapper.fmapper9b[T1,T2,T3,T4,T5,T6,T7,T8,T9](h)
  }
  
  implicit def fmapperFactory9c[T1,T2,T3,T4,T5,T6,T7,T8,T9] = new FMapperFactory[HParamsElems.RootParams.HParamsElem9[T1,T2,T3,T4,T5,T6,T7,T8,T9], (T1,T2,T3,T4,T5,T6,T7,T8,T9), FMapper.TPar[RootParams]#Type] {
    def apply(h:HParamsElems.RootParams.HParamsElem9[T1,T2,T3,T4,T5,T6,T7,T8,T9]) = FMapper.fmapper9c[T1,T2,T3,T4,T5,T6,T7,T8,T9](h)
  }
  
  implicit def fmapperFactory9d[T1,T2,T3,T4,T5,T6,T7,T8,T9] = new FMapperFactory[HPathElems.HPathElem9[T1,T2,T3,T4,T5,T6,T7,T8,T9], (T1,T2,T3,T4,T5,T6,T7,T8,T9), FMapper.TPath] {
    def apply(h:HPathElems.HPathElem9[T1,T2,T3,T4,T5,T6,T7,T8,T9]) = FMapper.fmapper9d[T1,T2,T3,T4,T5,T6,T7,T8,T9](h)
  }
  
  implicit def fmapperFactory9e[T1,T2,T3,T4,T5,T6,T7,T8,T9] = new FMapperFactory[HPathAndParamsElems.HPathAndParamsElem9[T1,T2,T3,T4,T5,T6,T7,T8,T9], (T1,T2,T3,T4,T5,T6,T7,T8,T9), FMapper.TPathAndPar] {
    def apply(h:HPathAndParamsElems.HPathAndParamsElem9[T1,T2,T3,T4,T5,T6,T7,T8,T9]) = FMapper.fmapper9e[T1,T2,T3,T4,T5,T6,T7,T8,T9](h)
  }

  implicit def fmapperFactory10a[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10] = new FMapperFactory[HParamsElems.RootPath.HParamsElem10[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10), FMapper.TPathAndPar] {
    def apply(h:HParamsElems.RootPath.HParamsElem10[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10]) = FMapper.fmapper10a[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10](h)
  }
  
  implicit def fmapperFactory10b[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10] = new FMapperFactory[HParamsElems.RootUri.HParamsElem10[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10), FMapper.TPar[RootUri]#Type] {
    def apply(h:HParamsElems.RootUri.HParamsElem10[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10]) = FMapper.fmapper10b[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10](h)
  }
  
  implicit def fmapperFactory10c[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10] = new FMapperFactory[HParamsElems.RootParams.HParamsElem10[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10), FMapper.TPar[RootParams]#Type] {
    def apply(h:HParamsElems.RootParams.HParamsElem10[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10]) = FMapper.fmapper10c[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10](h)
  }
  
  implicit def fmapperFactory10d[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10] = new FMapperFactory[HPathElems.HPathElem10[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10), FMapper.TPath] {
    def apply(h:HPathElems.HPathElem10[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10]) = FMapper.fmapper10d[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10](h)
  }
  
  implicit def fmapperFactory10e[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10] = new FMapperFactory[HPathAndParamsElems.HPathAndParamsElem10[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10), FMapper.TPathAndPar] {
    def apply(h:HPathAndParamsElems.HPathAndParamsElem10[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10]) = FMapper.fmapper10e[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10](h)
  }

  implicit def fmapperFactory11a[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11] = new FMapperFactory[HParamsElems.RootPath.HParamsElem11[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11), FMapper.TPathAndPar] {
    def apply(h:HParamsElems.RootPath.HParamsElem11[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11]) = FMapper.fmapper11a[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11](h)
  }
  
  implicit def fmapperFactory11b[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11] = new FMapperFactory[HParamsElems.RootUri.HParamsElem11[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11), FMapper.TPar[RootUri]#Type] {
    def apply(h:HParamsElems.RootUri.HParamsElem11[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11]) = FMapper.fmapper11b[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11](h)
  }
  
  implicit def fmapperFactory11c[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11] = new FMapperFactory[HParamsElems.RootParams.HParamsElem11[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11), FMapper.TPar[RootParams]#Type] {
    def apply(h:HParamsElems.RootParams.HParamsElem11[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11]) = FMapper.fmapper11c[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11](h)
  }
  
  implicit def fmapperFactory11d[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11] = new FMapperFactory[HPathElems.HPathElem11[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11), FMapper.TPath] {
    def apply(h:HPathElems.HPathElem11[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11]) = FMapper.fmapper11d[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11](h)
  }
  
  implicit def fmapperFactory11e[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11] = new FMapperFactory[HPathAndParamsElems.HPathAndParamsElem11[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11), FMapper.TPathAndPar] {
    def apply(h:HPathAndParamsElems.HPathAndParamsElem11[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11]) = FMapper.fmapper11e[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11](h)
  }

  implicit def fmapperFactory12a[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12] = new FMapperFactory[HParamsElems.RootPath.HParamsElem12[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12), FMapper.TPathAndPar] {
    def apply(h:HParamsElems.RootPath.HParamsElem12[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12]) = FMapper.fmapper12a[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12](h)
  }
  
  implicit def fmapperFactory12b[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12] = new FMapperFactory[HParamsElems.RootUri.HParamsElem12[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12), FMapper.TPar[RootUri]#Type] {
    def apply(h:HParamsElems.RootUri.HParamsElem12[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12]) = FMapper.fmapper12b[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12](h)
  }
  
  implicit def fmapperFactory12c[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12] = new FMapperFactory[HParamsElems.RootParams.HParamsElem12[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12), FMapper.TPar[RootParams]#Type] {
    def apply(h:HParamsElems.RootParams.HParamsElem12[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12]) = FMapper.fmapper12c[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12](h)
  }
  
  implicit def fmapperFactory12d[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12] = new FMapperFactory[HPathElems.HPathElem12[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12), FMapper.TPath] {
    def apply(h:HPathElems.HPathElem12[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12]) = FMapper.fmapper12d[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12](h)
  }
  
  implicit def fmapperFactory12e[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12] = new FMapperFactory[HPathAndParamsElems.HPathAndParamsElem12[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12), FMapper.TPathAndPar] {
    def apply(h:HPathAndParamsElems.HPathAndParamsElem12[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12]) = FMapper.fmapper12e[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12](h)
  }

  implicit def fmapperFactory13a[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13] = new FMapperFactory[HParamsElems.RootPath.HParamsElem13[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13), FMapper.TPathAndPar] {
    def apply(h:HParamsElems.RootPath.HParamsElem13[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13]) = FMapper.fmapper13a[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13](h)
  }
  
  implicit def fmapperFactory13b[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13] = new FMapperFactory[HParamsElems.RootUri.HParamsElem13[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13), FMapper.TPar[RootUri]#Type] {
    def apply(h:HParamsElems.RootUri.HParamsElem13[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13]) = FMapper.fmapper13b[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13](h)
  }
  
  implicit def fmapperFactory13c[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13] = new FMapperFactory[HParamsElems.RootParams.HParamsElem13[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13), FMapper.TPar[RootParams]#Type] {
    def apply(h:HParamsElems.RootParams.HParamsElem13[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13]) = FMapper.fmapper13c[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13](h)
  }
  
  implicit def fmapperFactory13d[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13] = new FMapperFactory[HPathElems.HPathElem13[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13), FMapper.TPath] {
    def apply(h:HPathElems.HPathElem13[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13]) = FMapper.fmapper13d[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13](h)
  }
  
  implicit def fmapperFactory13e[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13] = new FMapperFactory[HPathAndParamsElems.HPathAndParamsElem13[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13), FMapper.TPathAndPar] {
    def apply(h:HPathAndParamsElems.HPathAndParamsElem13[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13]) = FMapper.fmapper13e[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13](h)
  }

  implicit def fmapperFactory14a[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14] = new FMapperFactory[HParamsElems.RootPath.HParamsElem14[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14), FMapper.TPathAndPar] {
    def apply(h:HParamsElems.RootPath.HParamsElem14[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14]) = FMapper.fmapper14a[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14](h)
  }
  
  implicit def fmapperFactory14b[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14] = new FMapperFactory[HParamsElems.RootUri.HParamsElem14[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14), FMapper.TPar[RootUri]#Type] {
    def apply(h:HParamsElems.RootUri.HParamsElem14[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14]) = FMapper.fmapper14b[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14](h)
  }
  
  implicit def fmapperFactory14c[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14] = new FMapperFactory[HParamsElems.RootParams.HParamsElem14[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14), FMapper.TPar[RootParams]#Type] {
    def apply(h:HParamsElems.RootParams.HParamsElem14[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14]) = FMapper.fmapper14c[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14](h)
  }
  
  implicit def fmapperFactory14d[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14] = new FMapperFactory[HPathElems.HPathElem14[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14), FMapper.TPath] {
    def apply(h:HPathElems.HPathElem14[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14]) = FMapper.fmapper14d[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14](h)
  }
  
  implicit def fmapperFactory14e[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14] = new FMapperFactory[HPathAndParamsElems.HPathAndParamsElem14[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14), FMapper.TPathAndPar] {
    def apply(h:HPathAndParamsElems.HPathAndParamsElem14[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14]) = FMapper.fmapper14e[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14](h)
  }

  implicit def fmapperFactory15a[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15] = new FMapperFactory[HParamsElems.RootPath.HParamsElem15[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15), FMapper.TPathAndPar] {
    def apply(h:HParamsElems.RootPath.HParamsElem15[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15]) = FMapper.fmapper15a[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15](h)
  }
  
  implicit def fmapperFactory15b[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15] = new FMapperFactory[HParamsElems.RootUri.HParamsElem15[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15), FMapper.TPar[RootUri]#Type] {
    def apply(h:HParamsElems.RootUri.HParamsElem15[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15]) = FMapper.fmapper15b[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15](h)
  }
  
  implicit def fmapperFactory15c[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15] = new FMapperFactory[HParamsElems.RootParams.HParamsElem15[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15), FMapper.TPar[RootParams]#Type] {
    def apply(h:HParamsElems.RootParams.HParamsElem15[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15]) = FMapper.fmapper15c[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15](h)
  }
  
  implicit def fmapperFactory15d[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15] = new FMapperFactory[HPathElems.HPathElem15[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15), FMapper.TPath] {
    def apply(h:HPathElems.HPathElem15[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15]) = FMapper.fmapper15d[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15](h)
  }
  
  implicit def fmapperFactory15e[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15] = new FMapperFactory[HPathAndParamsElems.HPathAndParamsElem15[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15), FMapper.TPathAndPar] {
    def apply(h:HPathAndParamsElems.HPathAndParamsElem15[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15]) = FMapper.fmapper15e[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15](h)
  }

  implicit def fmapperFactory16a[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16] = new FMapperFactory[HParamsElems.RootPath.HParamsElem16[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16), FMapper.TPathAndPar] {
    def apply(h:HParamsElems.RootPath.HParamsElem16[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16]) = FMapper.fmapper16a[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16](h)
  }
  
  implicit def fmapperFactory16b[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16] = new FMapperFactory[HParamsElems.RootUri.HParamsElem16[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16), FMapper.TPar[RootUri]#Type] {
    def apply(h:HParamsElems.RootUri.HParamsElem16[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16]) = FMapper.fmapper16b[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16](h)
  }
  
  implicit def fmapperFactory16c[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16] = new FMapperFactory[HParamsElems.RootParams.HParamsElem16[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16), FMapper.TPar[RootParams]#Type] {
    def apply(h:HParamsElems.RootParams.HParamsElem16[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16]) = FMapper.fmapper16c[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16](h)
  }
  
  implicit def fmapperFactory16d[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16] = new FMapperFactory[HPathElems.HPathElem16[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16), FMapper.TPath] {
    def apply(h:HPathElems.HPathElem16[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16]) = FMapper.fmapper16d[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16](h)
  }
  
  implicit def fmapperFactory16e[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16] = new FMapperFactory[HPathAndParamsElems.HPathAndParamsElem16[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16), FMapper.TPathAndPar] {
    def apply(h:HPathAndParamsElems.HPathAndParamsElem16[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16]) = FMapper.fmapper16e[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16](h)
  }

  implicit def fmapperFactory17a[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17] = new FMapperFactory[HParamsElems.RootPath.HParamsElem17[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17), FMapper.TPathAndPar] {
    def apply(h:HParamsElems.RootPath.HParamsElem17[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17]) = FMapper.fmapper17a[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17](h)
  }
  
  implicit def fmapperFactory17b[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17] = new FMapperFactory[HParamsElems.RootUri.HParamsElem17[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17), FMapper.TPar[RootUri]#Type] {
    def apply(h:HParamsElems.RootUri.HParamsElem17[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17]) = FMapper.fmapper17b[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17](h)
  }
  
  implicit def fmapperFactory17c[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17] = new FMapperFactory[HParamsElems.RootParams.HParamsElem17[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17), FMapper.TPar[RootParams]#Type] {
    def apply(h:HParamsElems.RootParams.HParamsElem17[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17]) = FMapper.fmapper17c[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17](h)
  }
  
  implicit def fmapperFactory17d[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17] = new FMapperFactory[HPathElems.HPathElem17[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17), FMapper.TPath] {
    def apply(h:HPathElems.HPathElem17[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17]) = FMapper.fmapper17d[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17](h)
  }
  
  implicit def fmapperFactory17e[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17] = new FMapperFactory[HPathAndParamsElems.HPathAndParamsElem17[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17), FMapper.TPathAndPar] {
    def apply(h:HPathAndParamsElems.HPathAndParamsElem17[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17]) = FMapper.fmapper17e[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17](h)
  }

}