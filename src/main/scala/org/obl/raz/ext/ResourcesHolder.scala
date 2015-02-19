package org.obl.raz
package ext

class BaseResource(protected val prefix: Seq[String]) extends BasePath[IsRelativePath, CanAddPath, CanHavePathAsPrefix](None, PathSg(prefix), Nil, None) {
  def this(prefix: String) = this(Seq(prefix))
}

trait ResourceHolder {

  def currentHost:Option[PathBase]
  
  class BaseResource(protected val prefix: Seq[String]) extends BasePath[IsRelativePath, CanAddPath, CanHavePathAsPrefix](currentHost, PathSg(prefix), Nil, None) {
    def this(prefix: String) = this(Seq(prefix))
  }  

}

