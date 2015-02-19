package org.obl.raz

import scalaz.{-\/, \/, \/-}
import exceptions.PathExpectationException

object PathUtils {

	def mergeAll(pths:Seq[Path]):Path = {
    pths.headOption match {
      case None => Path.empty
      case Some(phd) => {
        var p = PathSg(phd.path.path)
        val q = collection.mutable.Buffer.empty[QParamSg] ++ phd.params
        val paths = pths.tail
        paths.foreach { pth =>
          p = p.add(pth.path)
          q ++= pth.params
        }
        Path(phd.base,p,q, paths.lastOption.flatMap(_.fragment))   
      }
    }
  }
	
  def merge(pths:Path*):Path = {
    mergeAll(pths)
  }
  
  def subtract(from:Path, what:Path):Throwable \/ Path  = {
    what match {
      case Path(_, wpath, pars, _) if (pars.isEmpty) => {
    	  if (from.path.path.startsWith(wpath.path)) \/-(Path(None, PathSg( from.path.path.drop(wpath.path.length)), from.params, None))
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
          \/-(Path(None, PathSg.empty, remainingPars, None))
        } else -\/(PathExpectationException(what, from))
      }
    }
  }
  
}