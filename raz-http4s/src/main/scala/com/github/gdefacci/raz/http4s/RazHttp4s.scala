package com.github.gdefacci.raz
package http4s

import org.http4s.{Request, Uri}
import scalaz.\/

object RazHttp4s  {
  
  implicit val http4sExtPathDecode:PathExtractor[Request]  = new PathExtractor[Request] {
    def apply(i:Request):Throwable \/ Path = \/.fromTryCatchNonFatal( fromHttp4s(i.uri).get )
  }
  
  implicit val http4sExtPathDecode1:PathExtractor[org.http4s.dsl.Path] = new PathExtractor[org.http4s.dsl.Path] {
    def apply(i:org.http4s.dsl.Path) = \/.fromTryCatchNonFatal( Path(None, None, i.toList, Nil, None) )
  }
  
  private def fromHttp4s(uri:Uri): Option[Path] = {
    val pth = uri.path.split("/").filter(_.trim.nonEmpty)
    val params: Seq[(String, Option[String])] = uri.params.map { e =>
      e._1 -> Option(e._2)
    }.toSeq
    Some(uri.fragment match {
      case Some(f) => Path(None, None, pth, params, Some(f))
      case None => Path(None, None, pth, params, None)
    })
  }
  
}