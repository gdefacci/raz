package org.obl.raz

trait FragmentAdd[A] {
  
  type Out
  
  def addFragment(a:A, fragment:String):Out
  
}

trait FragmentAddAux[A, O] extends FragmentAdd[A] {
  
  final type Out = O
  
  def addFragment(a:A, fragment:String):Out
  
}

object FragmentAdd {
  
  import PathPosition._
  
  private def apply[A,O]( f:(A, String) => O) = new FragmentAddAux[A,O] {
    def addFragment(a:A, fragment:String):O = f(a,fragment)
  }
  
  implicit def fragmentAddTPath[S <: PathPosition, E <: PathPosition](implicit sgPathApp:PathAppender[E,PathPosition.Fragment]) =
    apply[TPath[S,E], TPath[S, PathPosition.Fragment]]( (pth, par) => pth.append(Path.fragment(par)) )

  implicit def fragmentAddTUriTemplate[S <: PathPosition, E <: PathPosition](implicit sgPathApp:PathAppender[E,Fragment]) =
    apply[TUriTemplate[S,E], TUriTemplate[S, PathPosition.Fragment]]( (a, segment) => a.append(UriTemplate.fragment(segment)) )
    
  implicit def fragmentAddPathEncoder[T,S <: PathPosition, E <: PathPosition](implicit sgPathApp:PathAppender[E,PathPosition.Fragment]) = 
    apply[PathEncoder[T,S,E], PathEncoder[T,S,PathPosition.Fragment]](( pe, par ) => RightPathEncoder(pe, Path.fragment(par)))

  implicit def fragmentAddPathDecoder[T,S <: PathPosition, E <: PathPosition](implicit sgPathApp:PathAppender[E,PathPosition.Fragment]) = 
    apply[PathDecoder[T,S,E], PathDecoder[T,S,PathPosition.Fragment]]( (pd, par) => RightPathDecoder(pd, Path.fragment(par)) )

  implicit def fragmentAddPathCodec[TD,TE,S <: PathPosition, E <: PathPosition](implicit sgPathApp:PathAppender[E,PathPosition.Fragment]) = 
    apply[PathCodec[TD,TE,S,E], PathCodec[TD,TE,S,PathPosition.Fragment]]( (pc, par) => pc.append(Path.fragment(par)) )
    
  implicit def fragmentAddPathConverter[TD,TE,TU,S <: PathPosition, E <: PathPosition](implicit sgPathApp:PathAppender[E,PathPosition.Fragment]) = 
    apply[PathConverter[TD,TE,TU,S,E], PathConverter[TD,TE,TU,S,PathPosition.Fragment]]( (pc, par) => pc.append(Path.fragment(par)) )
    
  import shapeless._
  import shapeless.ops.hlist._
    
  implicit def fragmentAddToHCons1[T](implicit sgAdd:FragmentAdd[T]) = 
    apply[T :: HNil, sgAdd.Out :: HNil]( (a, par) =>  sgAdd.addFragment(a.head, par) :: HNil )
  
  implicit def fragmentAddToHCons[T, H <: HList, HR <: HList](implicit sgAdd:FragmentAddAux[H, HR]) = 
    apply[T :: H, T :: sgAdd.Out]( (a, par) =>  a.head :: sgAdd.addFragment(a.tail, par) )
  
}