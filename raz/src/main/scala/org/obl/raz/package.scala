package org.obl

import language.implicitConversions

package object raz {

  import shapeless.HList
  
  implicit def toPathOps[H <: HList](p:H) = new PathOps[H](p)  
}
