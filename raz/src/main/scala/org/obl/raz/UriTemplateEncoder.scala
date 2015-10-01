package org.obl.raz

import scala.language.implicitConversions
import scala.language.higherKinds 

trait UriTemplateEncoder[UT] {
  type UTEncoder[T] <: UriTemplateEncoder[T] 
  
  def toUriTemplate(t:UT):UriTemplate
  
  protected def createUriTemplateEncoder[T1](f:T1 => UriTemplate):UTEncoder[T1]
  
  def uriTemplateContramap[T1](f:T1 => UT):UTEncoder[T1] = {
    createUriTemplateEncoder[T1]((toUriTemplate _) compose f)
  }
}

object UriTemplateEncoder {
  def apply[T](f:T => UriTemplate):UriTemplateEncoder[T] = {
    new UriTemplateEncoder[T] {
      type UTEncoder[T] = UriTemplateEncoder[T] 
      def toUriTemplate(t:T) = f(t)
      protected def createUriTemplateEncoder[T1](f:T1 => UriTemplate) = UriTemplateEncoder.apply[T1](f)
    }
  }
  
  def opt[T](enc:UriTemplateEncoder[T]):UriTemplateEncoder[Option[T]] = {
    UriTemplateEncoder[Option[T]]( sq => UriTemplateUtils.mergeAll(sq.map(enc.toUriTemplate(_)).toSeq) )
  }
  
  private def makePlaceholdersExpandeded(ut:UriTemplate):UriTemplate = {
    UriTemplate(ut.base, ut.path.map {
      case PlaceHolder(nm) => ExpandPlaceHolder(nm)
      case x => x
    }, ut.params.map {
      case PlaceHolder(nm) => ExpandPlaceHolder(nm)
      case x => x
    }, ut.fragment.map {
      case PlaceHolder(nm) => ExpandPlaceHolder(nm)
      case x => x
    })    
  }
  
  def expand[T](enc:UriTemplateEncoder[T]):UriTemplateEncoder[T] = {
    UriTemplateEncoder[T]( sq => makePlaceholdersExpandeded( enc.toUriTemplate(sq) ))
  }
  
  private[raz] def withSuffix[T](e:UriTemplateEncoder[T], sfx:Path) = {
    UriTemplateEncoder[T]( v => UriTemplateUtils.merge( e.toUriTemplate(v) ,UriTemplate(sfx) ))
  }
  
  object Simple {
    
    def segment = UriTemplateEncoder[String](nm => UriTemplate.create(path = PlaceHolder(nm) :: Nil))
    def param = UriTemplateEncoder[String](nm => UriTemplate.create(params = PlaceHolder(nm) :: Nil))
    def paramNamed(str:String) = UriTemplateEncoder[String](nm => UriTemplate.create(params =  UriTemplateParamImpl(UriTemplateParamNameImpl(str), PlaceHolder(nm)) :: Nil))
    def fragment = UriTemplateEncoder[String](nm => UriTemplate.create(fragment = Some(PlaceHolder(nm))))
    
  }

  implicit def apply[H <: HPath, UT](h: H)(implicit uthf: UTHPathF[H, UT]): UriTemplateEncoder[UT] = {
    val ute = uthf.apply(h)
    UriTemplateEncoder(ute)
  }
}
