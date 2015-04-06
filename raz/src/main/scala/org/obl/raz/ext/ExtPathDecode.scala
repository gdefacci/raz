package org.obl.raz
package ext

trait ExtPathDecode[I] {
  def apply(i:I):Path
}

trait ExtUnapply {
  
  def unapply[U,D](p:U)(implicit pathMatcher:PathMatcher[this.type, D], extPathDecode:ext.ExtPathDecode[U]):Option[D] = {
    pathMatcher.decoder(this).decodeFull(extPathDecode(p)).toOption
  }
  
}