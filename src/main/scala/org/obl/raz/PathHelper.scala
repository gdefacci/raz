package org.obl.raz

object PathHelper {

  def merge(paths:Path*):Path = {
    var p = PathSg.empty
    val q = collection.mutable.Buffer.empty[QParamSg]
    paths.foreach { pth =>
      p = p.add(pth.path)
      q ++= pth.params
    }
    Path(paths.headOption.flatMap(_.base),p,q, paths.lastOption.flatMap(_.fragment))
  }
  
  /**
   * if <code>what</code> is a prefix of <code>from</code> then returns the path obtained stripping the prefix <code>what</code> from <code>from</code> rapped in <code>Some</code>, 
   * otherwise None is returned
   */
  def subtract(from:Path, what:Path):Option[Path] = {
    what match {
      case Path(_, wpath, pars, _) if (pars.isEmpty) => {
    	  if (from.path.path.startsWith(wpath.path)) Some(Path(None, PathSg( from.path.path.drop(wpath.path.length)), from.params, None))
    	  else None
      }
      case Path(_,wpath, wpars,_) => {
        if (from.path.path == wpath.path) {
          val remainingPars = collection.mutable.Buffer.empty[QParamSg]
          var it = from.params.iterator
          
          while (it.hasNext) {
            val nxt = it.next
            
            if (wpars.indexOf(nxt) < 0)  {
              remainingPars += nxt
            } 
          }
          Some(Path(None, PathSg.empty, remainingPars, None))
        } else None
      }
    }
  }
  

}