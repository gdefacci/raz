package org.obl.raz.servlet

import org.obl.raz._
import scalaz.{-\/, \/, \/-}

import scala.language.implicitConversions

import javax.servlet.http.HttpServletRequest


object Servlet  {

  implicit def servletExtPathDecode = new ext.ExtPathDecode[HttpServletRequest] {
    def apply(i:HttpServletRequest):Path = fromServlet(i).toOption.get
  }
  
  def fromServlet(req: HttpServletRequest): Throwable \/ Path = {
    val host = req.getRemoteHost
    val pathBase = \/.fromTryCatch( req.getRemotePort.toInt ).flatMap { port =>
      req.getProtocol.toUpperCase match {
        case "HTTP" => \/-(HTTP(host, port))
        case "HTTPS" => \/-(HTTPS(host, port))
        case x => -\/(new UnsupportedOperationException(s"unsupported protocol $x"))
      }
    }
    pathBase.map { pb =>
      val start = pb.render.length
      val end = req.getRequestURL.lastIndexOf("?")
      val pathStr = req.getRequestURL.substring(start, end)
      val parsStr = req.getRequestURL.substring(end)
      val sgs = PathSg(pathStr.split("/").filter(_.length > 0))
      val pars = parsStr.split(Array('&', ';')).map( parseParam(_) )
      Path(Some(pb), sgs, pars, None)  
    }
  }
  
  private def parseParam(str:String):QParamSg = {
    str.indexOf("=") match {
      case i if i < 0 => QParamSg(str)
      case i => {
        val nm = str.substring(0, i)
        val v = str.substring(i)
        QParamSg(nm, v)
      }
    }
  }
 
}