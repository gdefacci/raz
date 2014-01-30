package org.obl.raz

case class PathMatchResult[+T, +R](value:T, rest:R) {
  def mapValue[P,T1 >: T](f:T1 => P):PathMatchResult[P,R] = PathMatchResult[P,R](f(value), rest)
}

trait PathF[+T] {
  
  private[raz] def apply[T1 >: T](t:T1):Path
  private[raz] def matchPath(p:Path):Option[PathMatchResult[T, Path]]
  private[raz] def suffix:Path
//  private[raz] def fragment:Option[String]
  
  private[raz] def expansionKind:ExpansionKind.Value
  
  private[raz] def addPath(sg:PathSg):PathF[T]
  private[raz] def addParam(sg:QParamSg):PathF[T]
  private[raz] def withFragment(nm:String):PathF[T]
  
  private[raz] def addPaths(sgs:Seq[PathSg]) = {
    var r = this
    sgs.foreach( sg => r = r.addPath(sg))
    r
  }
  
  private[raz] def addParams(sgs:Seq[QParamSg]) = {
    var r = this
    sgs.foreach( sg => r = r.addParam(sg))
    r
  }
  
  private[raz] def merge(p:Path):PathF[T] = {
    var pv = this
    pv = pv.addPath(p.path)
    pv = pv.addParams(p.params)
    p.fragment.foreach  { frg =>
      pv = pv.withFragment(frg)  
    }
    pv
  }
  
  override def toString = {
    "PathF(suffix:"+suffix+")"
  }
  
}

object PathSgF {
  def apply[T](f:T => PathSg, matcher:PathSg => Option[PathMatchResult[T, PathSg]], expansionKind:ExpansionKind.Value) = new PathSgF(f,matcher,expansionKind)
}

class PathSgF[T](f:T => PathSg, matcher:PathSg => Option[PathMatchResult[T, PathSg]], expansionKind:ExpansionKind.Value) {
  def pathf = PathF.path[T](f, matcher, expansionKind)
}

object ParamSgF {
  def apply[T](f:T => Seq[QParamSg], matcher:Seq[QParamSg] => Option[PathMatchResult[T, Seq[QParamSg]]], expansionKind:ExpansionKind.Value) = new ParamSgF(f, matcher, expansionKind)
}


class ParamSgF[T](f:T => Seq[QParamSg], matcher:Seq[QParamSg] => Option[PathMatchResult[T, Seq[QParamSg]]], expansionKind:ExpansionKind.Value) {
  def pathf = PathF.param[T](f, matcher, expansionKind)
}

object PathF {

  def mapTo[T,P](pathf:PathF[T], cnv:Converter[T,P]):PathF[P] = {
    val mtchr = (pathf.matchPath _).andThen { opt =>
      opt.map { r =>
        r.mapValue(cnv.apply)
      }
    }
    new PathFImpl[P](p => cnv.unapply(p).map(pathf.apply).getOrElse(Path.empty), mtchr, pathf.suffix, pathf.expansionKind)
  }
  
  def apply[T](f:T => Path, matcher:Path => Option[PathMatchResult[T, Path]], suffix:Path, expansionKind:ExpansionKind.Value) = {
    new PathFImpl[T](f, matcher , suffix, expansionKind)
  }
  
  def pathMatcher[T](matcher:PathSg => Option[PathMatchResult[T, PathSg]]) = 
    (t:Path) => {
      matcher(t.path).map( r => PathMatchResult[T, Path](r.value, Path(None, r.rest, t.params, None)))
    }
  
  def path[T](f:T => PathSg, matcher:PathSg => Option[PathMatchResult[T, PathSg]], expansionKind:ExpansionKind.Value) = {
    apply[T](t => Path(None, f(t), Nil, None), pathMatcher(matcher), Path.empty, expansionKind)
  }
    
  def paramMatcher[T](matcher:Seq[QParamSg] => Option[PathMatchResult[T, Seq[QParamSg]]]) = {
      (t:Path) => matcher(t.params).map( r => PathMatchResult[T, Path](r.value, Path(None, t.path, r.rest, None))) 
    }
  
  def param[T](f:T => Seq[QParamSg], matcher:Seq[QParamSg] => Option[PathMatchResult[T, Seq[QParamSg]]], expansionKind:ExpansionKind.Value) = {
    apply[T](t => Path(None, PathSg.empty, f(t), None), t => matcher(t.params).map( r => PathMatchResult[T, Path](r.value, Path(None, t.path, r.rest, None))), Path.empty, expansionKind)
  }
  
}

class PathFImpl[+T](f:T=>Path, matcher:Path => Option[PathMatchResult[T, Path]], val suffix:Path, val expansionKind:ExpansionKind.Value) extends PathF[T]{
  private[raz] def apply[T1 >: T](t:T1):Path = {
    PathHelper.merge(f(t.asInstanceOf[T]), suffix)
  }
  
  def matchPath(p:Path):Option[PathMatchResult[T, Path]] = matcher(p).flatMap { r =>
    PathHelper.subtract(r.rest, suffix).map(rpth => PathMatchResult(r.value, rpth))
  }
  
  def addPath(sg:PathSg):PathF[T] = new PathFImpl[T](f,matcher, Path(suffix.base, suffix.path.add(sg), suffix.params, suffix.fragment), expansionKind)
  def addParam(sg:QParamSg) = new PathFImpl[T](f,matcher, Path(suffix.base, suffix.path, suffix.params ++ Seq(sg), suffix.fragment), expansionKind)
  
  def withFragment(frg:String) = new PathFImpl[T](f,matcher, Path(suffix.base, suffix.path, suffix.params, Some(frg)), expansionKind)
}

object PathFs {
  private[raz] def pathVarMatcher[T](sc:StringConverter[T]):PathSg=>Option[PathMatchResult[T,PathSg]] = { sg =>
    sc.unapply(sg.path.headOption).map { t =>
      PathMatchResult(t, PathSg(sg.path.tail))
    }
  }
  
  private[raz] def paramVarMatcher[T](matcher:QParamSg => Option[T]):Seq[QParamSg] => Option[PathMatchResult[T, Seq[QParamSg]]] = {
    { s:Seq[QParamSg] =>
      val it = s.iterator
      var res:Option[T] = None
      var rest = collection.mutable.Buffer.empty[QParamSg]
      while (it.hasNext) {
        val nxt = it.next
        if (!res.isEmpty) rest += nxt
        else {
          matcher(nxt) match {
            case None => rest += nxt
            case v @ Some(_) => res = v
          }
        }
      }
      res.map( PathMatchResult[T, Seq[QParamSg]](_, rest))
    }
  }
  
  private def paramVarSeqMatcher[T](matcher:QParamSg => Option[T]):Seq[QParamSg] => Option[PathMatchResult[Seq[T], Seq[QParamSg]]] = {
    { s:Seq[QParamSg] =>
      val it = s.iterator
      val res = collection.mutable.Buffer.empty[T]
      var rest = collection.mutable.Buffer.empty[QParamSg]
      while (it.hasNext) {
        val nxt = it.next
        matcher(nxt) match {
          case None => rest += nxt
          case Some(v) => res += v
        }
      }
      Some(PathMatchResult[Seq[T], Seq[QParamSg]](res.toSeq, rest))
    }
  }
  
  private def paramVarOptMatcher[T](name:String, sc:StringConverter[T]):Seq[QParamSg] => Option[PathMatchResult[Option[T], Seq[QParamSg]]] = {
    { s:Seq[QParamSg] =>
      var (withSameName, othrs) = s.partition(_.name == name)
      
      val it = withSameName.iterator
      var res:Option[Option[T]] = None
      var rest = collection.mutable.Buffer.empty[QParamSg] ++ othrs
      while (it.hasNext) {
        val nxt = it.next
        val parValue = nxt.value
        if (res.isDefined) rest += nxt
        else if (parValue == Some("")) res = Some(None)
        else sc.unapply(parValue) match {
          case None => rest += nxt
          case sm @ Some(_) => res = Some(sm)
        }
      }
      if (res.isEmpty) res = Some(None)
      res.map(PathMatchResult[Option[T], Seq[QParamSg]](_, rest))
    }
  }
    
  def paramVar[T](f:T => Option[QParamSg], matcher:QParamSg => Option[T], expansionKind:ExpansionKind.Value) =
    ParamSgF[T](t => f(t).toSeq, paramVarMatcher(matcher), expansionKind)
    
  def paramVarSeq[T](f:T => Option[QParamSg], matcher:QParamSg => Option[T]) =
    ParamSgF[Seq[T]](ts => ts.map(f).collect { case Some(x) => x }, paramVarSeqMatcher(matcher), ExpansionKind.formStyleQueryExpansion)
    
  def pathVar[T](matcher:PathSg=>Option[PathMatchResult[T,PathSg]])(implicit f:StringConverter[T]) = 
    PathSgF[T](t => f(t).map(PathSg(_)).getOrElse(PathSg.empty), matcher, ExpansionKind.stringExpansion)
    
  def pathVar[T](implicit f:StringConverter[T]):PathSgF[T] =
    pathVar(pathVarMatcher(f))(f)

  def paramValueVar[T](name:String, matcher:QParamSg=>Option[T])(implicit f:StringConverter[T]):ParamSgF[T] = 
    paramVar[T](t => f(t).map( v => QParamSg(name, Some(v)) ), matcher, ExpansionKind.ParamValueStringExpansion(name))
  
  def paramSeqVar[T](name:String, matcher:QParamSg=>Option[T])(implicit f:StringConverter[T]):ParamSgF[Seq[T]] = 
    paramVarSeq[T](t => f(t).map(v => QParamSg(name,v)), matcher)
    
  def paramValueMatcher[T](name:String, f:StringConverter[T]):QParamSg=>Option[T] = (sg:QParamSg) => 
    if (sg.name==name) f.unapply(sg.value)
    else f.unapply(None)
    
  def paramValueVar[T](name:String)(implicit f:StringConverter[T]):ParamSgF[T] = 
    paramValueVar[T](name, paramValueMatcher(name, f)) 

  def paramSeqVar[T](name:String)(implicit f:StringConverter[T]):ParamSgF[Seq[T]] = 
    paramSeqVar[T](name, paramValueMatcher(name, f))
    
  def optParamVar[T](name:String)(implicit f:StringConverter[T]):ParamSgF[Option[T]] = 
    ParamSgF[Option[T]]({t => 
      t match {
        case Some(v) => Seq(QParamSg(name, f(v)))
        case _ => Nil
      }
    }, paramVarOptMatcher(name, f), ExpansionKind.ParamValueStringExpansion(name)) 

}