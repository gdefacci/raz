package org.obl.raz

trait Mapper[H] {
  type Out
  
  def apply(h:H):Out
  
}

object Mapper {
  
  implicit def fromFMapperA[H,T1](implicit fm:FMapperFactory[H, T1, FMapper.TPathAndPar]) = new Mapper[H]  {
    type Out = FMapper[H, T1, FMapper.TPathAndPar]
    def apply(h:H) = fm(h)
  }
  
  implicit def fromFMapperB[H,T1](implicit fm:FMapperFactory[H, T1, FMapper.TPar[RootUri]#Type]) = new Mapper[H]  {
    type Out = FMapper[H, T1, FMapper.TPar[RootUri]#Type]
    def apply(h:H) = fm(h)
  }
  
  implicit def fromFMapperC[H,T1](implicit fm:FMapperFactory[H, T1, FMapper.TPar[RootParams]#Type]) = new Mapper[H]  {
    type Out = FMapper[H, T1, FMapper.TPar[RootParams]#Type]
    def apply(h:H) = fm(h)
  }
  
  implicit def fromFMapperD[H,T1](implicit fm:FMapperFactory[H, T1, FMapper.TPath]) = new Mapper[H]  {
    type Out = FMapper[H, T1, FMapper.TPath]
    def apply(h:H) = fm(h)
  }
  
}