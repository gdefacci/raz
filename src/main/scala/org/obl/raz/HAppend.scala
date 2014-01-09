package org.obl.raz

import PathHelper._

trait HAppender[H <: Resource, H1 <: Resource, Out <: Resource] {
  def concat(h: H, h1: H1): Out
}

object HAppender {

  implicit object HAppender1 extends HAppender[RootPath, RootPath, RootPath] {
    def concat(h: RootPath, h1: RootPath) = RootPath(h.path.add(h1.path))
  }
  
  implicit object HAppender1a extends HAppender[RootPath, RootParams, RootUri] {
    def concat(h: RootPath, h1: RootParams) = RootUri(h.value, h1.params)
  }
  
  implicit object HAppender1b extends HAppender[RootParams, RootParams, RootParams] {
    def concat(h: RootParams, h1: RootParams) = RootParams(h.params ++ h1.params)
  }

  implicit object HAppender2 extends HAppender[RootPath, RootUri, RootUri] {
    def concat(h: RootPath, h1: RootUri) = RootUri(h.path.add(h1.path), h1.params)
  }
  
  implicit object HAppender2a extends HAppender[RootUri, RootParams, RootUri] {
    def concat(h: RootUri, h1: RootParams) = RootUri(h.value, h.params ++ h1.params)
  }
  
  
  class HAppender5a0[T1, H1 <: PathResource] extends HAppender[PathHResource[H1, T1], RootPath, PathHResource[H1, T1]] {
    def concat(ph:PathHResource[H1, T1], h1:RootPath) = {
      val h = ph.addPath(h1.path)
      new PathHResource(h.head, h.value)
    }
  }
  
  class HAppender5a1[T1, H1 <: PathResource] extends HAppender[PathHResource[H1, T1], RootUri, PathAndParamsHResource[H1, T1]] {
    def concat(ph:PathHResource[H1, T1], h1:RootUri) = {
      val h = ph.addPath(h1.path)
      new PathAndParamsHResource(h.head, h.value.addParams(h1.params)  )
    }
  }
  
  class HAppender5a[T1, H1 <: PathResource] extends HAppender[PathHResource[H1, T1], RootParams, PathAndParamsHResource[H1, T1]] {
    def concat(h:PathHResource[H1, T1], h1:RootParams) = {
      new PathAndParamsHResource(h.head, h.value.addParams(h1.params))
    }
  }

  implicit def toHAppender5a0[T1, H1 <: PathResource] = new HAppender5a0[T1, H1]
  implicit def toHAppender5a1[T1, H1 <: PathResource] = new HAppender5a1[T1, H1]
  
  implicit def toHAppender5a[T1, H1 <: PathResource] = new HAppender5a[T1,H1]
  
  class HAppender5b[T1, H1 <: PathResource] extends HAppender[PathAndParamsHResource[H1, T1], RootParams, PathAndParamsHResource[H1, T1]] {
    def concat(h:PathAndParamsHResource[H1, T1], h1:RootParams) = {
      new PathAndParamsHResource(h.head, h.value.addParams(h1.params))
    }
  }
  
  implicit def toHAppender5b[T1, H1 <: PathResource] = new HAppender5b[T1,H1]
  
  class HAppender5c[T1, H1 <: Resource] extends HAppender[ParamsHResource[H1, T1], RootParams, ParamsHResource[H1, T1]] {
    def concat(h:ParamsHResource[H1, T1], h1:RootParams) = {
      new ParamsHResource(h.head, h.value.addParams(h1.params))
    }
  }
  
  implicit def toHAppender5c[T1, H1 <: Resource] = new HAppender5c[T1,H1]
  
  implicit def toHAppender4[T2, H1 <: PathResource, H2 <: PathResource, Out <: PathResource](implicit appender:HAppender[H1,H2,Out]) = new HAppender4[T2,H1,H2,Out](appender)
//  implicit def toHAppender4_0[T2, H1 <: PathResource, H2 <: Resource, Out <: Resource](implicit appender:HAppender[H1,H2,Out]) = new HAppender4_0[T2,H1,H2,Out](appender)
//  implicit def toHAppender4_1[T2, H1 <: PathResource, H2 <: PathResource, Out <: Resource](appender:HAppender[H1,H2,Out]) = new HAppender4_1[T2,H1,H2,Out](appender)
  implicit def toHAppender4a[T2, H1 <: Resource, H2 <: PathResource, Out <: PathResource](implicit appender:HAppender[H1,H2,Out]) = new HAppender4a[T2,H1,H2,Out](appender)
  implicit def toHAppender4b[T2, H1 <: Resource, H2 <: Resource, Out <: Resource](implicit appender:HAppender[H1,H2,Out]) = new HAppender4b[T2,H1,H2,Out](appender)
  
  class HAppender4[T2, H1 <: PathResource, H2 <: PathResource, Out <: PathResource](appender:HAppender[H1,H2,Out]) extends HAppender[H1, PathHResource[H2, T2], PathHResource[Out, T2]] {
    def concat(h1:H1, h2:PathHResource[H2, T2]) = new PathHResource[Out, T2](appender.concat(h1, h2.head), h2.value)
  }
  
  class HAppender4_0[T2, H1 <: PathResource, H2 <: Resource, Out <: Resource](appender:HAppender[H1,H2,Out]) extends HAppender[H1, ParamsHResource[H2, T2], ParamsHResource[Out, T2]] {
    // class HAppender4_0 needs to be abstract, since method concat in trait HAppender of type 
    //        (h: H1, h1: org.obl.raz.PathHResource[H2,T2])org.obl.raz.PathAndParamsHResource[Out,T2] is not defined
    def concat(h1:H1, h2:ParamsHResource[H2, T2]) = new ParamsHResource[Out, T2](appender.concat(h1, h2.head), h2.value)
  }

//  class HAppender4_1[T2, H1 <: PathResource, H2 <: Resource, Out <: Resource](appender:HAppender[H1,H2,Out]) extends HAppender[H1, PathHResource[H2, T2], ParamsHResource[Out, T2]] {
//    def concat(h1:H1, h2:ParamsHResource[H2, T2]) = new PathHResource[Out, T2](appender.concat(h1, h2.head), h2.value)
//  }
  
  class HAppender4a[T2, H1 <: Resource, H2 <: PathResource, Out <: PathResource](appender:HAppender[H1,H2,Out]) extends HAppender[H1, PathAndParamsHResource[H2, T2], PathAndParamsHResource[Out, T2]] {
    def concat(h1:H1, h2:PathAndParamsHResource[H2, T2]) = new PathAndParamsHResource[Out, T2](appender.concat(h1, h2.head), h2.value)
  }
  
  class HAppender4b[T2, H1 <: Resource, H2 <: Resource, Out <: Resource](appender:HAppender[H1,H2,Out]) extends HAppender[H1, ParamsHResource[H2, T2], ParamsHResource[Out, T2]] {
    def concat(h1:H1, h2:ParamsHResource[H2, T2]) = new ParamsHResource[Out, T2](appender.concat(h1, h2.head), h2.value)
  }
  
}


