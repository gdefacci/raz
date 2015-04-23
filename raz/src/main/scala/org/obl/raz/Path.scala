package org.obl.raz

import scala.language.implicitConversions
import scala.language.higherKinds

sealed trait Path extends PathRenderer with ext.ExtUnapply { 

  def path: PathSg
  def params: Seq[QParamSg]
  def fragment: Option[String]

  def isEmpty = path.isEmpty && params.isEmpty && fragment.isEmpty

  override def hashCode = Path.baseOf(this).hashCode + path.hashCode * 11 + params.hashCode * 17 + fragment.hashCode * 23

  override def equals(o: Any): Boolean = o match {
    case othr: Path => Path.baseOf(this) == Path.baseOf(othr) && path == othr.path && params.toSet == othr.params.toSet && fragment == othr.fragment
    case x => false
  }

  override final def toString = render

}

object Path  {
  
  def baseOf(path:Path):Option[PathBase] = 
    path match {
    case p : BasePath[_,_] => p.pathBase
  }

   def unapply(p:Path) = Some(baseOf(p), p.path, p.params, p.fragment)
 
  def ralativePath(p:Path):RelativePath[_,_] =
    p match {
     case p:RelativePath[_,_] => p
     case x => p.fragment match {
       case None => RelativePath(p.path, p.params)
       case Some(f) => RelativePath(p.path, p.params, f)
     }
    }
   
  def copy(p:Path)(path:PathSg = p.path, params:Seq[QParamSg] = p.params, fragment:Option[String] = p.fragment):Path =
    BasePath(baseOf(p), path, params, fragment)

  def apply(pathBase:Option[PathBase], path:PathSg, params:Seq[QParamSg], fragment:Option[String]):Path =
    BasePath(pathBase, path, params, fragment)
}

sealed trait PathPosition
sealed abstract class BasePosition extends PathPosition  
sealed abstract class SegmentPosition extends BasePosition 
sealed abstract class ParamPosition extends SegmentPosition 
sealed abstract class FragmentPosition extends ParamPosition 

sealed abstract class BasePath[P <: PathPosition,S <: P](private [raz] val pathBase:Option[PathBase], val path:PathSg, val params:Seq[QParamSg], val fragment:Option[String]) extends Path {
  def at[R](base:PathBase)(implicit atAux:AtAux[BasePath[P,S], R]):R = atAux.apply(this)(base)
} 

class RelativePath[P <: SegmentPosition,S <: P] private [raz] (path:PathSg, params:Seq[QParamSg], fragment:Option[String]) extends BasePath[P,S](None, path, params, fragment) {
  
  def at(base:PathBase):AbsolutePath[S] = new AbsolutePath[S](base, path, params, fragment) 
}

class AbsolutePath[S <: BasePosition] private [raz] (val base:PathBase, path:PathSg, params:Seq[QParamSg], fragment:Option[String]) extends BasePath[BasePosition, S](Some(base), path, params, fragment)

object BasePath {
  
  private [raz] def empty[P <: PathPosition, S <: P] = BasePath[P, S](None, PathSg.empty, Nil, None)
  
  def sum[P1 <: PathPosition, S1 <: P1, P2 <: S1, S2 <: P2](p1:BasePath[P1,S1], p2:BasePath[P2,S2]):BasePath[P1,S2] = {
    BasePath(Path.baseOf(p1).orElse(Path.baseOf(p2)), p1.path.add(p2.path), p1.params ++ p2.params, p1.fragment.orElse(p2.fragment)  )
  }
  
  private [raz] def apply[P <: PathPosition, S <: P](pathBase:Option[PathBase], path:PathSg, params:Seq[QParamSg], fragment:Option[String]):BasePath[P,S] = {
    val r = pathBase match {
      case Some(b) => fragment match { 
        case None => AbsolutePath(b, path, params)
        case Some(f) => AbsolutePath(b, path, params, f)
      }
      case None => fragment match {
        case None => RelativePath(path, params)
        case Some(f) => RelativePath(path, params, f)
      }
    }
    r.asInstanceOf[BasePath[P,S]]
  }

  implicit def toPathSegmentAdder[P >: SegmentPosition <: PathPosition](path:BasePath[P, SegmentPosition])  = new PathSegmentAdder[P] {
    lazy val segmentAdderSelf = path
  } 
  
  implicit def toParamAdder[P >: ParamPosition <: PathPosition, A >: ParamPosition <: P](path:BasePath[P,A]) = new ParamAdder[P,A] {
    def paramAdderSelf = path
  }
  
  implicit def toFragmentAdder[P >: FragmentPosition <: PathPosition, S >: ParamPosition <: P](path:BasePath[P,S]) = new FragmentAdder[P] {
    def fragmentAdderSelf = path
  }

  implicit def apply[P <: PathPosition, S <: P, R[_,_,_]](h:BasePath[P,S])(implicit pathConversion:ext.PathConversion[R]):R[Path,Path,Path] = {
    pathConversion(h)
  }
}

object RelativePath extends RelativePath[SegmentPosition, SegmentPosition](PathSg.empty, Nil, None) with EmptyPathAdder {
  
  def apply(path:PathSg) = 
    new RelativePath[SegmentPosition, SegmentPosition](path, Nil, None) 
    
  def apply(path:PathSg, params:Seq[QParamSg]) = 
   new RelativePath[SegmentPosition, ParamPosition](path, params, None) 

  def apply(path:PathSg, params:Seq[QParamSg], fragment:String) = 
    new RelativePath[SegmentPosition, FragmentPosition](path, params, Some(fragment)) 

  def apply(params:Seq[QParamSg]) = 
    new RelativePath[ParamPosition, ParamPosition](PathSg(Nil), params, None)  
    
  def apply(params:Seq[QParamSg], fragment:String) = 
    new RelativePath[ParamPosition, FragmentPosition](PathSg(Nil), params, Some(fragment)) 
  
  def apply(fragment:String) = 
    new RelativePath[ParamPosition, FragmentPosition](PathSg(Nil), Nil, Some(fragment)) 
    
}

object AbsolutePath {
  
  def apply(base:PathBase) = 
    new AbsolutePath[SegmentPosition](base, PathSg.empty, Nil, None) 
  
  def apply(base:PathBase, path:PathSg) = 
    new AbsolutePath[SegmentPosition](base, path, Nil, None) 
    
  def apply(base:PathBase, path:PathSg, params:Seq[QParamSg]) = 
    new AbsolutePath[ParamPosition](base, path, params, None)

  def apply(base:PathBase, path:PathSg, params:Seq[QParamSg], fragment:String) = 
    new AbsolutePath[FragmentPosition](base, path, params, Some(fragment))

  def apply(base:PathBase, params:Seq[QParamSg]) = 
    new AbsolutePath[ParamPosition](base, PathSg(Nil), params, None)
    
  def apply(base:PathBase, params:Seq[QParamSg], fragment:String) = 
    new AbsolutePath[FragmentPosition](base, PathSg(Nil), params, Some(fragment))
  
  def apply(base:PathBase, fragment:String) = 
    new AbsolutePath[FragmentPosition](base, PathSg(Nil), Nil, Some(fragment))
    
}
