package org.obl.raz
package ext

import scala.language.higherKinds 

trait PathConversion[Result[_,_,_]] {

  def apply[P <: PathPosition, S <: P](bp:BasePath[P,S]):Result[Path,Path,Path]
  
}