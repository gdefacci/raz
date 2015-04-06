package org.obl.raz

trait UriTemplateRenderer { self: UriTemplate =>

  private val paramEscape = java.net.URLEncoder.encode(_: String, "UTF-8")

  private def param(isFirst: Boolean, p: QParamSg): String = {
    val prfx = if (isFirst) "?" else ""
    val (name, value) = (p.name -> p.value)
    val sfx = value match {
      case None => paramEscape(name)
      case Some(v) => paramEscape(name) + "=" + paramEscape(v)
    }
    prfx+sfx
  }

  private def param(isFirst: Boolean, p: PlaceHolder) = {
    val prfx = if (isFirst) "{?" else "{&"
    val sfx = "}"
    s"$prfx${p.name}$sfx"
  }

  private def param(isFirst: Boolean, p: ExpandPlaceHolder) = {
    val prfx = if (isFirst) "{?" else "{&"
    val sfx = "*}"
    s"$prfx${p.name}$sfx"
  }

  private def param(isFirst: Boolean, p: UriTemplateParam): String = {
    p match {
      case UriTemplateParamSg(p) => param(isFirst, p)
      case pl @ PlaceHolder(p) => param(isFirst, pl)
      case lpl: ExpandPlaceHolder => param(isFirst, lpl)
      case UriTemplateParamImpl(n, v) => {
        val prfx = if (isFirst) "?" else ""
        val nm = n match {
          case UriTemplateParamNameImpl(nm) => paramEscape(nm)
          case PlaceHolder(nm) => s"{$nm}"
        }
        val value = v match {
          case UriTemplateParamValueImpl(vl) => paramEscape(vl)
          case PlaceHolder(nm) => s"{$nm}"
        }
        s"$prfx$nm=$value"
      }
    }
  }

  private def segment(s: PlaceHolder): String = {
    s"/{${s.name}}"
  }

  private def segment(s: ExpandPlaceHolder): String = {
    s"/{${s.name}*}"
  }

  private def segment(s: UriTemplatePathSegment): String = {
    s match {
      case UriTemplatePathSg(sg) if !sg.isEmpty => "/"+(sg.path.map(UriPartEncode.encode(UriPartEncode.pathUriTemplateUnescaped)).mkString("/"))
      case UriTemplatePathSg(_) => ""
      case pl: PlaceHolder => segment(pl)
      case lpl: ExpandPlaceHolder => segment(lpl)
    }
  }

  private def fragment(f: UriTemplateFragment) = f match {
    case UriTemplateFragmentImpl(frg) => "#" + frg
    case PlaceHolder(nm) => s"{#$nm}"
    case ExpandPlaceHolder(nm) => s"{#$nm*}"
  }

  def render: String = {
    val pars = {
      if (params.isEmpty) ""
      else {
        val pars = params.headOption.map(param(true, _)).toList ++ params.tail.map(param(false, _))
        pars.mkString("&")
      }
    }

    val sb = new StringBuilder()
    base.foreach(b => sb.append(b.render))

    if (!path.isEmpty) {
      val aux = path.map(segment)
      sb.append(aux.mkString(""))
    } else if (params.nonEmpty || fragment.nonEmpty) {
      sb.append("/")
    }
      
    sb.append(pars)

    fragment.foreach { frg =>
      sb.append(fragment(frg))
    }

    sb.toString
  }

}