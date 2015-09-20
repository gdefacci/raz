package org.obl.raz
package ext

trait ExtPathDecode[I] {
  def apply(i:I):Path
}

object ExtPathDecode {

  implicit lazy val identity:org.obl.raz.ext.ExtPathDecode[org.obl.raz.Path]  = new org.obl.raz.ext.ExtPathDecode[org.obl.raz.Path] {
    def apply(i:org.obl.raz.Path):org.obl.raz.Path = i
  }

}

trait ExtUnapply {
  
  def unapply[U,D](p:U)(implicit pathMatcher:PathMatcher[this.type, D], extPathDecode:ext.ExtPathDecode[U]):Option[D] = {
    pathMatcher.decoder(this).decodeFull(extPathDecode(p)).toOption
  }
  
}