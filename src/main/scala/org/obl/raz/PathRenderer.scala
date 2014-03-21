package org.obl.raz

trait PathRenderer { self: Path =>

  def render: String = {
    val pars = {
      if (params.isEmpty) ""
      else {
        "?" + (params.map { p =>
          val (name, value) = (p.name -> p.value)
          val escpr = java.net.URLEncoder.encode(_:String)
          value match {
            case None => escpr(name)
            case Some(v) => escpr(name) + "=" + escpr(v)
          }
        }.mkString("&"))
      }
    }

    val sb = new StringBuilder
    base.foreach { bs =>
      if (bs.endsWith("/")) sb.append(sb.substring(0, bs.length - 1))
      else sb.append(bs)
    }

    if (!path.isEmpty) {
      sb.append("/")
      sb.append(path.path.map(UriPartEncode.encode(UriPartEncode.pathUnescaped)).mkString("/"))
      sb.append(pars)
    } else {
      sb.append(pars)
    }

    fragment.foreach { frg =>
      sb.append("#" + frg)
    }

    sb.toString
  }
  
  private[raz] def renderUriTemplate: String = {
    val pars = {
      if (params.isEmpty) ""
      else {
        val r = (params.map { p =>
          val (name, value) = (p.name -> p.value)
          val escpr = UriPartEncode.encode(UriPartEncode.paramUriTemplateUnescaped)(_:String)
          value match {
            case None => escpr(name)
            case Some(v) => escpr(name) + "=" + escpr(v)
          }
        }.mkString("&"))
        if (r.startsWith("{")) r else "?"+r
      }
    }

    val sb = new StringBuilder
    base.foreach { bs =>
      if (bs.endsWith("/")) sb.append(sb.substring(0, bs.length - 1))
      else sb.append(bs)
    }

    if (!path.isEmpty) {
      sb.append("/")
      sb.append(path.path.map(UriPartEncode.encode(UriPartEncode.pathUriTemplateUnescaped)).mkString("/"))
      sb.append(pars)
    } else {
      sb.append(pars)
    }

    fragment.foreach { frg =>
      sb.append("#" + frg)
    }

    sb.toString
  }
}