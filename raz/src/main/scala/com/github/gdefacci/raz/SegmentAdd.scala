package com.github.gdefacci.raz

trait SegmentAdd[A] {
  
  type Out
  
  def addSegment(a:A, segment:String):Out
  
}

trait SegmentAddAux[A, O] extends SegmentAdd[A] {
  
  final type Out = O
  
  def addSegment(a:A, segment:String):Out
  
}

object SegmentAdd {
  
  import PathPosition._
  
  def apply[A,O]( f:(A, String) => O) = new SegmentAddAux[A,O] {
    def addSegment(a:A, sg:String):O = f(a,sg)
  }
  
  implicit def addTPath[S <: PathPosition, E <: PathPosition](implicit sgPathApp:PathAppender[E,Segment]) =
    apply[TPath[S,E], TPath[S, PathPosition.Segment]]( (a, segment) => a.append(Path.segments(segment)) )

  implicit def addTUriTemplate[S <: PathPosition, E <: PathPosition](implicit sgPathApp:PathAppender[E,Segment]) =
    apply[TUriTemplate[S,E], TUriTemplate[S, PathPosition.Segment]]( (a, segment) => a.append(UriTemplate.segments(segment)) )
    
  implicit def addPathEncoder[T,S <: PathPosition, E <: PathPosition](implicit sgPathApp:PathAppender[E,Segment]) = 
    apply[PathEncoder[T,S,E], PathEncoder[T,S,Segment]]( (a, segment) => RightPathEncoder(a, Path.segments(segment)))
  
  implicit def addPathDecoder[T,S <: PathPosition, E <: PathPosition](implicit sgPathApp:PathAppender[E,Segment]) = 
    apply[PathDecoder[T,S,E], PathDecoder[T,S,Segment]]( (a, segment) => RightPathDecoder(a, Path.segments(segment)) )
  
  implicit def addPathCodec[TD,TE,S <: PathPosition, E <: PathPosition](implicit sgPathApp:PathAppender[E,Segment]) = 
    apply[PathCodec[TD,TE,S,E], PathCodec[TD,TE,S,Segment]]( (pc, par) => pc.append(Path.segments(par)) )
  
  implicit def addPathConverter[TD,TE,TU,S <: PathPosition, E <: PathPosition](implicit sgPathApp:PathAppender[E,Segment]) = 
    apply[PathConverter[TD,TE,TU,S,E], PathConverter[TD,TE,TU,S,Segment]]( (a, segment) =>  a.append(Path.segments(segment)))
  
  import shapeless._
  import shapeless.ops.hlist._
    
  implicit def addToHCons1[T](implicit sgAdd:SegmentAdd[T]) = 
    apply[T :: HNil, sgAdd.Out :: HNil]( (a, segment) =>  sgAdd.addSegment(a.head, segment) :: HNil )
  
  implicit def addToHCons[T, H <: HList, HR <: HList](implicit sgAdd:SegmentAddAux[H, HR]) = 
    apply[T :: H, T :: sgAdd.Out]( (a, segment) =>  a.head :: sgAdd.addSegment(a.tail, segment) )
  
  
}