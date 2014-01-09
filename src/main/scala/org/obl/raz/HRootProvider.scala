package org.obl.raz

trait ToHRoot[H] {
  type Out
  
  def apply(h:H):Out
}

object ToHRoot {
  implicit def toHRoot[L <: Resource, R](implicit last : HRootProvider[L, R]) = new ToHRoot[L] {
    type Out = R
    def apply(l : L) : Out = last(l)
  }
}


trait HRootProvider[H <: Resource, R] {

  def apply(h: H): R
}

object HRootProvider {

  implicit object HRootProvider1 extends HRootProvider[RootPath, RootPath] {
    def apply(r: RootPath) = r
  }

  implicit object HRootProvider2 extends HRootProvider[RootUri, RootUri] {
    def apply(r: RootUri) = r
  }

  implicit object HRootProvider3 extends HRootProvider[RootParams, RootParams] {
    def apply(r: RootParams) = r
  }
  
  implicit def toHRootProvider1A[T]:HRootProvider[PathHResource[RootPath, T], RootPath] = new HRootProvider[PathHResource[RootPath, T], RootPath] {
    def apply(r: PathHResource[RootPath, T]) = r.head
  }
  
  implicit def toHRootProvider2A[T]:HRootProvider[PathAndParamsHResource[RootPath, T], RootPath] = new HRootProvider[PathAndParamsHResource[RootPath, T], RootPath] {
    def apply(r: PathAndParamsHResource[RootPath, T]) = r.head
  }
  
  implicit def toHRootProvider3A[T]:HRootProvider[ParamsHResource[RootParams, T], RootParams] = new HRootProvider[ParamsHResource[RootParams, T], RootParams] {
    def apply(r: ParamsHResource[RootParams, T]) = r.head
  }
  
  implicit def toHRootProvider3Ab[T]:HRootProvider[ParamsHResource[RootPath, T], RootPath] = new HRootProvider[ParamsHResource[RootPath, T], RootPath] {
    def apply(r: ParamsHResource[RootPath, T]) = r.head
  }
  
  implicit def toHRootProvider3Ac[T]:HRootProvider[ParamsHResource[RootUri, T], RootUri] = new HRootProvider[ParamsHResource[RootUri, T], RootUri] {
    def apply(r: ParamsHResource[RootUri, T]) = r.head
  }
  
  implicit def fromHElem[H <: PathResource, T](implicit rp: HRootProvider[H, RootPath]):HRootProvider[PathHResource[H, T], RootPath] = {
    new HRootProvider[PathHResource[H, T], RootPath] {

      def apply(h: PathHResource[H, T]) = rp(h.head)
    }
  }
  
  implicit def fromHElem1[H <: PathResource, T](implicit rp: HRootProvider[H, RootPath]):HRootProvider[PathAndParamsHResource[H, T], RootPath] = {
    new HRootProvider[PathAndParamsHResource[H, T], RootPath] {

      def apply(h: PathAndParamsHResource[H, T]) = rp(h.head)
    }
  }
  
  implicit def fromHElem2[H <: Resource, T](implicit rp: HRootProvider[H, RootPath]):HRootProvider[ParamsHResource[H, T], RootPath] = {
    new HRootProvider[ParamsHResource[H, T], RootPath] {

      def apply(h: ParamsHResource[H, T]) = rp(h.head)
    }
  }
  
  implicit def fromHElem3[H <: Resource, T](implicit rp: HRootProvider[H, RootUri]):HRootProvider[ParamsHResource[H, T], RootUri] = {
    new HRootProvider[ParamsHResource[H, T], RootUri] {

      def apply(h: ParamsHResource[H, T]) = rp(h.head)
    }
  }
  
  implicit def fromHElem4[H <: Resource, T](implicit rp: HRootProvider[H, RootParams]):HRootProvider[ParamsHResource[H, T], RootParams] = {
    new HRootProvider[ParamsHResource[H, T], RootParams] {

      def apply(h: ParamsHResource[H, T]) = rp(h.head)
    }
  }

}


