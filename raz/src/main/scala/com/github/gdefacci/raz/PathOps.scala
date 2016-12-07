package com.github.gdefacci.raz

class PathOps[P](self:P) {
  
  def / (segment:String)(implicit sgAdd:SegmentAdd[P]):sgAdd.Out = sgAdd.addSegment(self, segment)

  def / [T](segment:T)(implicit sgAdd:PathVarAdd[P, T], startsWith:StartsWith[T, PathPosition.Segment]):sgAdd.Out = sgAdd.add(self, segment)
    
  def && (par:String)(implicit sgAdd:ParamAdd[P]) = sgAdd.addParam(self, par -> None)  
  
  def && (name:String, value:String)(implicit sgAdd:ParamAdd[P]) = sgAdd.addParam(self, name -> Some(value))  
  
  def && [T](par:T)(implicit sgAdd:PathVarAdd[P, T], startsWith:StartsWith[T, PathPosition.Param]):sgAdd.Out = sgAdd.add(self, par)
  
  def &#(frament:String)(implicit fragAdd:FragmentAdd[P]) = fragAdd.addFragment(self, frament)  
  
  def &# [T](par:T)(implicit sgAdd:PathVarAdd[P, T]):sgAdd.Out = sgAdd.add(self, par)
  
  def pathEncoder[T, S0 <: PathPosition, E <: PathPosition](implicit tpe:ToPathEncoder[P,T,S0,E]) = tpe(self)
  
  def uriTemplateEncoder[T, S0 <: PathPosition, E <: PathPosition](implicit tue:ToUriTemplateEncoder[P,T,S0,E]):UriTemplateEncoder[T, S0, E] = tue(self)
    
  def pathDecoder[TUP, S0 <: PathPosition, E <: PathPosition](implicit hr: ToPathDecoder[P,TUP,S0,E]):PathDecoder[TUP, S0, E] = hr(self)

  def pathCodec[TD,TE,S0 <: PathPosition, E <: PathPosition](implicit pcd:ToPathCodec[P, TD, TE, S0, E]) = pcd(self)
  
  def pathConverter[TD,TE,TU, S0 <: PathPosition, E <: PathPosition](implicit pc:ToPathConverter[P, TD, TE, TU, S0, E]) = pc(self)
  
  import shapeless.ops.hlist._
  
  def ++ [H](t:H)(implicit pathConcat:PathConcat[P,H]):pathConcat.Out = pathConcat.concat(self,t)
  
}
