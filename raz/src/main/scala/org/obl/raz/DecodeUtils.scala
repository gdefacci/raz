package org.obl.raz

import scalaz.{ -\/, \/, \/- }
import java.net.URLDecoder

private[raz] object DecodeUtils {

  private val percentTripletSize = 3
  private val hexadecimal = 16

  def replacePercentTriplets(s: String): String = {
    var needToChange = false;
    val encoding: String = "UTF-8"
    val numChars = s.length();
    val sb = new StringBuilder(numChars);

    var i = 0
    while (i < numChars) {
      var c = s.charAt(i)
      c match {
        case '%' =>
          needToChange = true
          var pos = 0
          val bytes = Array.ofDim[Byte]((numChars - i) / percentTripletSize)
          while ((i + percentTripletSize - 1) < numChars && c == '%') {
            val v = Integer.parseInt(s.substring(i + 1, i + percentTripletSize), hexadecimal);
            if (v < 0)
              throw new IllegalArgumentException("Illegal hex characters in escape (%) pattern - negative value");

            bytes(pos) = v.asInstanceOf[Byte]
            pos += 1
            i += percentTripletSize

            if (i < numChars)
              c = s.charAt(i);
          }

          if ((i < numChars) && (c == '%'))
            throw new IllegalArgumentException(
              "Incomplete trailing escape (%) pattern");

          sb.append(new String(bytes, 0, pos, encoding));
        case x =>
          sb.append(c)
          i += 1
      }
    }

    if (needToChange) sb.toString else s
  }

  def subtract(from: Path, what: Path): Throwable \/ Path = {
    PathDecoder.path(what).decode(from).map(mr => mr.rest)
  }

  def subtract(from: UriTemplate, prefix: Path): Throwable \/ UriTemplate = subtract(from, UriTemplate(prefix))

  def subtract(from: UriTemplate, prefix: UriTemplate): Throwable \/ UriTemplate = {
    lazy val error = -\/(new RuntimeException(s"${prefix.render} is not a prefix of ${from.render}"))
    if (from.scheme != prefix.scheme || from.authority != prefix.authority) error
    else prefix match {

      case UriTemplate(_, _, segments, pars, fragment) if pars.isEmpty && fragment.isEmpty =>
        if (from.segments.startsWith(segments)) \/-(from.copy(scheme = None, authority = None, segments = from.segments.drop(segments.length)))
        else error

      case UriTemplate(_, _, segments, pars, fragment) if from.segments == segments =>
        val remainingPars = collection.mutable.Buffer.empty[UriTemplate.Param]
        var it = from.params.iterator

        while (it.hasNext) {
          val nxt = it.next

          if (pars.indexOf(nxt) < 0) {
            remainingPars += nxt
          }
        }

        fragment match {
          case None => \/-(from.copy(scheme = None, authority = None, segments = Nil, params = remainingPars))
          case Some(frag) if remainingPars.isEmpty && fragment == from.fragment => \/-(UriTemplate)
          case Some(frag) => error
        }

      case _ => error
    }
  }

  private def paramDecode = URLDecoder.decode(_: String, "UTF-8")

  private def parseParams(queryString: String): Throwable \/ Seq[(String, Option[String])] = {
    val zpars: Throwable \/ Seq[(String, Option[String])] = \/-(Nil)
    Option(queryString) match {
      case None => zpars
      case Some(q) =>
        q.split("&").filter(_.nonEmpty).toSeq.foldLeft(zpars) { (acc, itm) =>
          acc.flatMap { rpars =>
            itm.split("=").filter(_.nonEmpty).toSeq match {
              case Seq(k) => \/-(rpars :+ (paramDecode(k) -> None))
              case Seq(k, v) => \/-(rpars :+ (paramDecode(k) -> Some(paramDecode(v))))
              case _ => -\/(new RuntimeException(s"invalid argument pair $itm"))
            }
          }
        }
    }
  }

  def fromJavaUrl(u: java.net.URL): Throwable \/ Path = {

    val params = parseParams(u.getQuery)

    val frg = Option(u.getRef).map(DecodeUtils.replacePercentTriplets)
    for (scheme <- Scheme.fromString(u.getProtocol); pars <- params) yield Path(Some(scheme),
      Some(Authority(u.getHost, if (u.getPort > 0) u.getPort else 80)),
      u.getPath.split("/").filter(_.nonEmpty).map(DecodeUtils.replacePercentTriplets).toList,
      pars,
      frg)
  }

  def fromJavaUri(uri: java.net.URI): Throwable \/ Path = {
    for {
      scheme <- Option(uri.getScheme) match {
        case None => \/-(None)
        case Some(v) => Scheme.fromString(uri.getScheme).map(Some(_))
      }
      port = uri.getPort
      authority = Option(uri.getHost).map(hst => Authority(hst, if (port < 0) 80 else port))
      path = uri.getPath
      pth = if (path.startsWith("/")) path.substring(1) else path
      pathParts = pth.split("/").filter(_.length > 0).map(replacePercentTriplets(_))
      params <- parseParams(uri.getRawQuery)
      fragment = Option(uri.getFragment)
    } yield (Path(scheme, authority, pathParts.toList, params, fragment))

  }
}