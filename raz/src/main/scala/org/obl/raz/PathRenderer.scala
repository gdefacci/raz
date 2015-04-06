package org.obl.raz

trait PathRenderer { self: Path =>

  def render: String = {
    val pars = {
      if (params.isEmpty) ""
      else {
        "?" + (params.map { p =>
          val (name, value) = (p.name -> p.value)
          val escpr = java.net.URLEncoder.encode(_:String, "UTF-8")
          value match {
            case None => escpr(name)
            case Some(v) => escpr(name) + "=" + escpr(v)
          }
        }.mkString("&"))
      }
    }

    val sb = new StringBuilder()
    Path.baseOf(this).foreach(b => sb.append(b.render))

    if (!path.isEmpty) {
      sb.append("/")
      sb.append(path.path.map(UriPartEncode.encode(UriPartEncode.pathUnescaped)).mkString("/"))
    } else if (params.nonEmpty || fragment.nonEmpty) {
      sb.append("/")
    }
    
    sb.append(pars)

    fragment.foreach { frg =>
      sb.append("#" + frg)
    }

    sb.toString
  }
  
}