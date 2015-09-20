package org.obl.raz.http4s

import org.http4s.{Request, Uri}
import org.obl.raz.{RelativePath, PathSg}

object RazHttp4s  {
  
  implicit val http4sExtPathDecode:org.obl.raz.ext.ExtPathDecode[Request]  = new org.obl.raz.ext.ExtPathDecode[Request] {
    def apply(i:Request):org.obl.raz.Path = fromHttp4s(i.uri).get
  }
  
  implicit val http4sExtPathDecode1:org.obl.raz.ext.ExtPathDecode[org.http4s.dsl.Path] = new org.obl.raz.ext.ExtPathDecode[org.http4s.dsl.Path] {
    def apply(i:org.http4s.dsl.Path):org.obl.raz.Path = RelativePath(PathSg(i.toList))
  }
  
  private def fromHttp4s(uri:Uri): Option[org.obl.raz.Path] = {
    val pth = PathSg(uri.path.split("/").filter(_.trim.nonEmpty))
    val params: Seq[org.obl.raz.QParamSg] = uri.params.map { e =>
      org.obl.raz.QParamSg(e._1, Some(e._2))
    }.toSeq
    Some(uri.fragment match {
      case Some(f) => RelativePath(pth, params, f)
      case None => RelativePath(pth, params)
    })
  }
  
}