package org.obl.raz.http4s

import org.obl.raz
import org.obl.raz.PathExtractor
import org.http4s.{Request, Uri}
import scalaz.\/

object RazHttp4s  {
  
  implicit val http4sExtPathDecode:PathExtractor[Request]  = new PathExtractor[Request] {
    def apply(i:Request):Throwable \/ raz.Path = \/.fromTryCatchNonFatal( fromHttp4s(i.uri).get )
  }
  
  implicit val http4sExtPathDecode1:PathExtractor[org.http4s.dsl.Path] = new PathExtractor[org.http4s.dsl.Path] {
    def apply(i:org.http4s.dsl.Path) = \/.fromTryCatchNonFatal( raz.Path(None, None, i.toList, Nil, None) )
  }
  
  private def fromHttp4s(uri:Uri): Option[org.obl.raz.Path] = {
    val pth = uri.path.split("/").filter(_.trim.nonEmpty)
    val params: Seq[(String, Option[String])] = uri.params.map { e =>
      e._1 -> Option(e._2)
    }.toSeq
    Some(uri.fragment match {
      case Some(f) => raz.Path(None, None, pth, params, Some(f))
      case None => raz.Path(None, None, pth, params, None)
    })
  }
  
}