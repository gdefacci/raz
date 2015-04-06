package org.obl.raz

import Path.baseOf

trait BasePathSegmentAdder[P >: SegmentPosition <: PathPosition] {
  protected def segmentAdderSelf:BasePath[P, SegmentPosition] 
  
  def add(sg:String):BasePath[P, SegmentPosition] = BasePath[P, SegmentPosition](baseOf(segmentAdderSelf), segmentAdderSelf.path.add(sg), segmentAdderSelf.params, segmentAdderSelf.fragment)
  def / (sg:String) = add(sg)
  
  def add[D,E,UT,S <: SegmentPosition](pf: PathConverter[D,E,UT,SegmentPosition, S]) = HPathConsFactory[P].create(HPathNil(segmentAdderSelf), pf)
  def /[D,E,UT,S <: SegmentPosition](pf: PathConverter[D,E,UT,SegmentPosition,S])  = add(pf)
  
} 

trait PathSegmentAdder[P >: SegmentPosition <: PathPosition] extends BasePathSegmentAdder[P] {
  
  def append[P1 <: P, PA <: P1](p:BasePath[P1, PA]) =
    BasePath[P, PA](baseOf(segmentAdderSelf), PathSg(segmentAdderSelf.path.path ++ p.path.path), segmentAdderSelf.params ++ p.params, p.fragment)
    
  def ++ [P1  <: P, PA <: P1](p:BasePath[P1, PA]) = append[P1,PA](p)
  
  def append[H <: HPath, Out <: HPath](p:H)(implicit happ:HAppend[HPathNil[P, SegmentPosition], H, Out]) =
    happ.concat(HPathNil(segmentAdderSelf), p)
  
  def ++ [H <: HPath, Out <: HPath](p:H)(implicit happ:HAppend[HPathNil[P, SegmentPosition], H, Out]) = append[H,Out](p)  
    
}

trait BaseParamAdder[P >: ParamPosition <: PathPosition, S <: P ] {

  protected def paramAdderSelf:BasePath[P,S]
  
  def addParam(nm:String, value:String) = BasePath[P,ParamPosition](baseOf(paramAdderSelf), paramAdderSelf.path, paramAdderSelf.params ++ Seq(QParamSg(nm, value)), paramAdderSelf.fragment)
  def && (nm:String, value:String) = addParam(nm, value)
  
  def addParam[D,E,UT, S1 <: ParamPosition](pf: PathConverter[D,E,UT,ParamPosition,S1]) = 
    HPathConsFactory[P].create(HPathNil[P, S](paramAdderSelf), pf)
  def &&[D,E,UT, S1 <: ParamPosition](pf: PathConverter[D,E,UT,ParamPosition,S1])  = addParam(pf)
  
}

trait ParamAdder[P >: ParamPosition <: PathPosition, S <: P ] extends BaseParamAdder[P,S] {
  
  def append[P1 <: S, PA <: P1](p:BasePath[P1, PA]) =
    BasePath[P1, PA](baseOf(paramAdderSelf), PathSg(paramAdderSelf.path.path ++ p.path.path), paramAdderSelf.params ++ p.params, p.fragment)
    
  def ++ [P1 <: S, PA <: P1](p:BasePath[P1, PA]) = append[P1,PA](p)
  
  def append[H <: HPath, Out <: HPath](p:H)(implicit happ:HAppend[HPathNil[P,S], H, Out]) =
    happ.concat(HPathNil(paramAdderSelf), p)
  
  def ++ [H <: HPath, Out <: HPath](p:H)(implicit happ:HAppend[HPathNil[P,S], H, Out]) = append[H,Out](p)

}

trait FragmentAdder[P >: FragmentPosition <: PathPosition]  {
  
  def fragmentAdderSelf:BasePath[P,_]
  
  def addFragment(frg:String) = BasePath[P, FragmentPosition](baseOf(fragmentAdderSelf), fragmentAdderSelf.path, fragmentAdderSelf.params, Some(frg))
  def &# (str:String) = addFragment(str)
}


trait EmptyPathAdder extends BasePathSegmentAdder[SegmentPosition] with BaseParamAdder[ParamPosition,ParamPosition] {
  
  protected def segmentAdderSelf:BasePath[SegmentPosition, SegmentPosition] = BasePath[SegmentPosition, SegmentPosition](None, PathSg.empty, Nil, None)
  protected def paramAdderSelf:BasePath[ParamPosition, ParamPosition] = BasePath[ParamPosition, ParamPosition](None, PathSg.empty, Nil, None)
  
}