package org.obl.raz

object PathHelper {

  def sum(paths:Path*):Path = {
    var p = PathSg.empty
    val q = collection.mutable.Buffer.empty[QParamSg]
    paths.foreach { pth =>
      p = p.add(pth.path)
      q ++= pth.params
    }
    Path(p,q)
  }
  
  /**
   * if <code>what</code> is a prefix of <code>from</code> then returns the path obtained stripping the prefix <code>what</code> from <code>from</code> rapped in <code>Some</code>, 
   * otherwise None is returned
   */
  def subtract(from:Path, what:Path):Option[Path] = {
    what match {
      case Path(wpath, pars) if (pars.isEmpty) => {
    	  if (from.path.path.startsWith(wpath.path)) Some(Path(PathSg( from.path.path.drop(wpath.path.length)), from.params))
    	  else None
      }
      case Path(wpath, wpars) => {
        if (from.path.path == wpath.path) {
          val remainingPars = collection.mutable.Buffer.empty[QParamSg]
          var it = from.params.iterator
          
          while (it.hasNext) {
            val nxt = it.next
            
            if (wpars.indexOf(nxt) < 0)  {
              remainingPars += nxt
            } 
          }
          Some(Path(PathSg.empty, remainingPars))
        } else None
      }
    }
  }
  

}