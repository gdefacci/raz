package org.obl.raz

import scalaz.{-\/, \/, \/-}
import exceptions.PathExpectationException

object PathUtils {

	def mergeAll(pths:Seq[Path]):Path = {
    pths.headOption match {
      case None => BasePath.empty
      case Some(phd) => {
        var p = PathSg(phd.path.path)
        val q = collection.mutable.Buffer.empty[QParamSg] ++ phd.params
        val paths = pths.tail
        paths.foreach { pth =>
          p = p.add(pth.path)
          q ++= pth.params
        }
        val rp = paths.lastOption.flatMap(_.fragment) match {
          case None => RelativePath(p,q)
          case Some(frag) => RelativePath(p,q, frag)
        }
        
        Path.baseOf(phd).map( base => rp.at(base)).getOrElse(rp)
      }
    }
  }
	
  def merge(pths:Path*):Path = {
    mergeAll(pths)
  }
  
  def subtract(from:Path, what:Path):Throwable \/ Path  = {
    what match {
      case Path(_, wpath, pars, _) if (pars.isEmpty) => {
    	  if (from.path.path.startsWith(wpath.path)) \/-(RelativePath(PathSg( from.path.path.drop(wpath.path.length)), from.params))
    	  else -\/(PathExpectationException(what, from))
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
          \/-(RelativePath(remainingPars))
        } else -\/(PathExpectationException(what, from))
      }
    }
  }
  
}