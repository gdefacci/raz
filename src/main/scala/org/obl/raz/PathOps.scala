package org.obl.raz

import unfiltered.request._
import unfiltered.request.{ Path => UPath, Params => UParams }

case class PathOps[H <: Resource](helem:H) {

  def root[R <: HRoot](implicit rp:ToHRoot[H]):rp.Out = rp(helem)

  def append[H1 <: Resource, Out <: Resource](h1: H1)(implicit hpapender: HAppender[H, H1, Out]): Out = hpapender.concat(helem, h1)
  
//  def matcher(implicit pm:PathMatch[H]):Path => Option[PathMatchResult[pm.T, Path]] = pm(helem)
  def matcher[T](implicit pm:PathMatcher[H,T]):Path => Option[PathMatchResult[T, Path]] = pm.matcher(helem)
  
  def toF[F](implicit hf:HF[H,F]):F = hf(helem)
  
  def mapper(implicit mp:Mapper[H]) = mp(helem)
  
  def toUriTemplate[O](implicit uth:UTH[H,O]):O = uth(helem, UriTemplateFactory)
  
}
