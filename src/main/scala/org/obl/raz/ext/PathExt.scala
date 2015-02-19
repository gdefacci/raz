package org.obl.raz
package ext

import unfiltered.request.HttpRequest
import scalaz.{-\/, \/, \/-}

class PathExtApply[D,E](pathConverter:PathConverter[D,E,_,_,_]) {
  
  def apply(t:E):Path = pathConverter.encode(t)
  def unapply[REQ](req: HttpRequest[REQ]): Option[D] = {
    FromUnfiltered.toPath(req).flatMap( p => pathConverter.decodeFull(p).toOption )
  }
  
  def decodeFull(path:Path):Throwable \/ D = pathConverter.decodeFull(path)


  def map[T1](f:D => T1):PathExtApply[T1,E] = new PathExtApply[T1, E](pathConverter.map((f)))
  
  def contramap[T1](f:T1 => E):PathExtApply[D,T1] = new PathExtApply[D,T1](pathConverter.contramap((f)))

}

class PathExt[T](pathConverter:PathConverter[T,T,_,_,_]) extends PathExtApply[T,T](pathConverter) {
  
  def caseMap[T1](tupled: T => T1, caseUnapply: T1 => Option[T]) = new PathExt[T1](pathConverter.caseMap(tupled, caseUnapply))
}

object PathExt {
  
  
  private def emptyUTHPathF[H <: HPath, Any] = new UTHPathF[H, Any] {

    def apply(h:H):Any => UriTemplate = a => UriTemplate.empty
    
  }

  
  def apply[T](pathConverter:PathConverter[T,T,_,_,_]) = new PathExt[T](pathConverter)
 
//  implicit def apply[H <: HPath, R <: RelativePathAspect, A <: CanAddAspect, P <: CanHavePrefixAspect, T, UT](h: H)(implicit pathMatcher: PathMatcher[H, T], hf: EncHPathF[H, T, BasePath[R, A, P]]): PathExt[T] = {
//    implicit val uthf: UTHPathF[H, UT] = emptyUTHPathF
//    PathExt[T](PathConverter(h))
//  }
  
   implicit def apply[H <: HPath, T, UT](h: H)(implicit pathMatcher: PathMatcher[H, T], hf: EncHPathF[H, T, Path]): PathExt[T] = {
    implicit val uthf: UTHPathF[H, UT] = emptyUTHPathF
    val d = pathMatcher.decoder(h)
    val e = hf.apply(h)
    val ute = uthf.apply(h)
    PathExt[T](PathConverter(d, PathEncoder(e), UriTemplateEncoder(ute)))
  }
  
   def apply[H <: HPath, T1, T, UT]
    (p: AbstractHPathSegmentAdder[H, RelativePathAspect, CanAddPath, CanHavePathAsPrefix, T1, T1, UT])
    (implicit hf: EncHPathF[HPathCons[H, RelativePathAspect, CanAddPath, CanHavePathAsPrefix, T1, T1, UT], T, Path], pm: PathMatcher[HPathCons[H, RelativePathAspect, CanAddPath, CanHavePathAsPrefix, T1, T1, UT], T]): PathExt[T] = {
    PathExt(p.path)
  }
 
  
  def apply(pth:Path):PathExt[Path] = PathExt(PathConverter.fromPath(pth))

  
}