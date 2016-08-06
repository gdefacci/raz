package org.obl.raz

trait ParamAdd[A] {
  
  type Out
  
  def addParam(a:A, par:(String, Option[String])):Out
  
}

trait ParamAddAux[A, O] extends ParamAdd[A] {
  
  final type Out = O
  
  def addParam(a:A, par:(String, Option[String])):Out
  
}

object ParamAdd {
  
  import PathPosition._
  
  def apply[A,O]( f:(A, (String, Option[String])) => O) = new ParamAddAux[A,O] {
    def addParam(a:A, par:(String, Option[String])):O = f(a,par)
  }
  
  implicit def paramAddTPath[S <: PathPosition, E <: PathPosition](implicit sgPathApp:PathAppender[E,PathPosition.Param]) =
    apply[TPath[S,E], TPath[S, PathPosition.Param]]( (pth, par) => pth.append(Path.params(par)) )
    
  implicit def paramAddTUriTemplate[S <: PathPosition, E <: PathPosition](implicit sgPathApp:PathAppender[E,Param]) =
    apply[TUriTemplate[S,E], TUriTemplate[S, PathPosition.Param]]( (a, segment) => a.append(UriTemplate.params(segment)) )
  
  implicit def paramAddPathEncoder[T,S <: PathPosition, E <: PathPosition](implicit sgPathApp:PathAppender[E,PathPosition.Param]) = 
    apply[PathEncoder[T,S,E], PathEncoder[T,S,PathPosition.Param]](( pe, par ) => RightPathEncoder(pe, Path.params(par)))

  implicit def paramAddPathDecoder[T,S <: PathPosition, E <: PathPosition](implicit sgPathApp:PathAppender[E,PathPosition.Param]) = 
    apply[PathDecoder[T,S,E], PathDecoder[T,S,PathPosition.Param]]( (pd, par) => RightPathDecoder(pd, Path.params(par)) )

  implicit def paramAddPathCodec[TD,TE,S <: PathPosition, E <: PathPosition](implicit sgPathApp:PathAppender[E,PathPosition.Param]) = 
    apply[PathCodec[TD,TE,S,E], PathCodec[TD,TE,S,PathPosition.Param]]( (pc, par) => pc.append(Path.params(par)) )
    
  implicit def paramAddPathConverter[TD,TE,TU,S <: PathPosition, E <: PathPosition](implicit sgPathApp:PathAppender[E,PathPosition.Param]) = 
    apply[PathConverter[TD,TE,TU,S,E], PathConverter[TD,TE,TU,S,PathPosition.Param]]( (pc, par) => pc.append(Path.params(par)) )
    
  import shapeless._
  import shapeless.ops.hlist._
    
  implicit def paramAddToHCons1[T](implicit sgAdd:ParamAdd[T]) = 
    apply[T :: HNil, sgAdd.Out :: HNil]( (a, par) =>  sgAdd.addParam(a.head, par) :: HNil )
  
  implicit def paramAddToHCons[T, H <: HList, HR <: HList](implicit sgAdd:ParamAddAux[H, HR]) = 
    apply[T :: H, T :: sgAdd.Out]( (a, par) =>  a.head :: sgAdd.addParam(a.tail, par) )
  
}