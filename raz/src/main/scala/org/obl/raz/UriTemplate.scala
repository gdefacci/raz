package org.obl.raz

sealed trait UriTemplatePart

sealed trait UriTemplatePathSegment extends UriTemplatePart
final case class UriTemplatePathSg(path:PathSg) extends UriTemplatePathSegment

sealed trait UriTemplateParam extends UriTemplatePart

final case class UriTemplateParamSg(param:QParamSg) extends UriTemplateParam

sealed trait UriTemplateParamName extends UriTemplatePart
final case class UriTemplateParamNameImpl(value:String) extends UriTemplateParamName

sealed trait UriTemplateParamValue extends UriTemplatePart
final case class UriTemplateParamValueImpl(value:String) extends UriTemplateParamValue

final case class UriTemplateParamImpl(name:UriTemplateParamName, value:UriTemplateParamValue) extends UriTemplateParam

sealed trait UriTemplateFragment extends UriTemplatePart

final case class UriTemplateFragmentImpl(fragment:String) extends UriTemplateFragment

final case class PlaceHolder(name:String) extends UriTemplatePathSegment with UriTemplateParam with UriTemplateParamName with UriTemplateParamValue with UriTemplateFragment
final case class ExpandPlaceHolder(name:String) extends UriTemplatePathSegment with UriTemplateParam with UriTemplateFragment 

final case class UriTemplate(base:Option[PathBase], path:Seq[UriTemplatePathSegment], params:Seq[UriTemplateParam], fragment:Option[UriTemplateFragment]) extends UriTemplateRenderer
  
object UriTemplate {
  lazy val empty = UriTemplate(None,Nil,Nil,None)
  
  def create(base:Option[PathBase] = None, path:Seq[UriTemplatePathSegment] = Nil, params:Seq[UriTemplateParam] = Nil, fragment:Option[UriTemplateFragment] = None) =
    UriTemplate(base, path, params, fragment)

  def apply(p:Path):UriTemplate  = {
    UriTemplate(Path.baseOf(p), UriTemplatePathSg(p.path) :: Nil, p.params.map(UriTemplateParamSg), p.fragment.map(UriTemplateFragmentImpl.apply(_)))
  }
  
}