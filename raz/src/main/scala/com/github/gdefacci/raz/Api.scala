package com.github.gdefacci.raz

import scalaz.{ -\/, \/, \/- }

object Api {

  trait PathDecoder[T] {

    def decode(path: Path): Throwable \/ MatchResult[T]

    def decodeFull(path: Path): Throwable \/ T 

    def map[T1](f: T => T1): PathDecoder[T1]

    def unapply[I](i: I)(implicit pathExtractor: PathExtractor[I]): Option[T] 

  }
  
  trait PathMatchDecoder extends PathDecoder[Path] {
    def path:Path
  }
  
  trait PathEncoder[T] {
  
    def encode(t: T): Path
    def contramap[T1](f: T1 => T):PathEncoder[T1] 
  
  }
  
  trait UriTemplateEncoder[T] {
  
    def encodeUriTemplate(t: T): UriTemplate
  
  }

  trait PathCodec[TD,TE] extends PathDecoder[TD] with PathEncoder[TE] with PartialPathDecoder[TD] {
    
    def fullPath:PathCodec[TD,TE]
    def decoder:PathDecoder[TD]
    def encoder:PathEncoder[TE]
    
    def caseMap[C](mf:TD => C)(cf:C => Option[TE]):PathCodec[C,C]

  }
  
  object PathCodec {
    type Symmetric[T] = PathCodec[T,T]
  }
  
  
  trait PathConverter[TD,TE,UT] extends PathCodec[TD,TE] with UriTemplateEncoder[UT]  {
    def uriTemplateEncoder:UriTemplateEncoder[UT]
    
    def fullPath:PathConverter[TD,TE,UT]
    
    def caseMap[C](mf:TD => C)(cf:C => Option[TE]):PathConverter[C,C,UT]
  }

  
  trait PartialPathDecoder[T] extends PathDecoder[T] {
    
    def fullPath:PathDecoder[T]
    
  }
  
}