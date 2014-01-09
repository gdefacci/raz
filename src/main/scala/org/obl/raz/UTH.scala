package org.obl.raz

trait UTH[-H, Out] {

  def apply(h: H, utFactory: UTFactory): Out
}

trait UTFactory {
  def expansion[T](pathf: PathF[T]): String => Path

  def utMatcher[T](pathf: PathF[T]): Path => Option[PathMatchResult[String, Path]]
}

object UTH {

  def apply[H, Out](f: (H, UTFactory) => Out): UTH[H, Out] =
    new UTH[H, Out] {
      def apply(h: H, utFactory: UTFactory) = f(h, utFactory)
    }

  implicit def uthRootHPathElem[T](implicit cnv:StringConverter[T]) = apply[PathHResource[RootPath, T], PathHResource[RootPath, String]] { (h, utf) =>
    new PathHResource[RootPath, String](h.head, PathF(utf.expansion(h.value), utf.utMatcher(h.value), h.value.expansionKind))
  }

  implicit def uthRootHPathAnParamsElem[T](implicit cnv:StringConverter[T]) = apply[PathAndParamsHResource[RootPath, T], PathAndParamsHResource[RootPath, String]] { (h, utf) =>
    new PathAndParamsHResource[RootPath, String](h.head, PathF(utf.expansion(h.value), utf.utMatcher(h.value), h.value.expansionKind))
  }

  implicit def uthRootPathHParamsElem[T](implicit cnv:StringConverter[T]) = apply[ParamsHResource[RootPath, T], ParamsHResource[RootPath, String]] { (h, utf) =>
    new ParamsHResource[RootPath, String](h.head, PathF(utf.expansion(h.value), utf.utMatcher(h.value), h.value.expansionKind))
  }

  implicit def uthRootUriHParamsElem[T](implicit cnv:StringConverter[T]) = apply[ParamsHResource[RootUri, T], ParamsHResource[RootUri, String]] { (h, utf) =>
    new ParamsHResource[RootUri, String](h.head, PathF(utf.expansion(h.value), utf.utMatcher(h.value), h.value.expansionKind))
  }

  implicit def uthRootParamsHParamsElem[T](implicit cnv:StringConverter[T]) = apply[ParamsHResource[RootParams, T], ParamsHResource[RootParams, String]] { (h, utf) =>
    new ParamsHResource[RootParams, String](h.head, PathF(utf.expansion(h.value), utf.utMatcher(h.value), h.value.expansionKind))
  }

  implicit def utfHPathElem[H <: PathResource, T, Out <: PathResource](implicit huth: UTH[H, Out], cnv:StringConverter[T]) = apply[PathHResource[H, T], PathHResource[Out, String]] { (h, utf) =>
    new PathHResource[Out, String](huth(h.head, utf), PathF(utf.expansion(h.value), utf.utMatcher(h.value), h.value.expansionKind))
  }

  implicit def utfHPathAndParamsElem[H <: PathResource, T, Out <: PathResource](implicit huth: UTH[H, Out]) = apply[PathAndParamsHResource[H, T], PathAndParamsHResource[Out, String]] { (h, utf) =>
    new PathAndParamsHResource[Out, String](huth(h.head, utf), PathF(utf.expansion(h.value), utf.utMatcher(h.value), h.value.expansionKind))
  }

  implicit def utfHParamsElem[H <: Resource, T, Out <: Resource](implicit huth: UTH[H, Out]) = apply[ParamsHResource[H, T], ParamsHResource[Out, String]] { (h, utf) =>
    new ParamsHResource[Out, String](huth(h.head, utf), PathF(utf.expansion(h.value), utf.utMatcher(h.value), h.value.expansionKind))
  }

}