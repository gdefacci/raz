package org.obl.raz

object PathRenderer {

  private lazy val paramEscape: String => String = java.net.URLEncoder.encode(_: String, "UTF-8")
  private lazy val segmentEscape: String => String = UriPartEncode.encode(UriPartEncode.pathUnescaped)

  def render(p: Path): String = {
    lazy val params: String =
      if (p.params.isEmpty)
        ""
      else
        "?" + p.params.map { case (name, value) => paramEscape(name) + value.map(v => "=" + paramEscape(v)).getOrElse("") }.mkString("&")

    p.scheme.map(s => s.render + ":").getOrElse("") +
    p.authority.map(_.render).getOrElse("") + "/" +
      p.segments.map(segmentEscape).mkString("/") +
      params +
      p.fragment.map(f => s"#${segmentEscape(f)}").getOrElse("")
  }

  def render(p: UriTemplate): String = {
    val params: String =
      p.params match {
        case Seq() => ""
        case (hd :: rest) =>
          val pars: Seq[String] = renderUriTemplateParam(true, hd) +: rest.map(renderUriTemplateParam(false, _))
          pars.mkString("")
      }

    p.scheme.map(s => s.render + ":").getOrElse("") +
    p.authority.map(_.render).getOrElse("") + "/" +
      p.segments.map(renderUriTemplateSegment).mkString("/") +
      params +
      p.fragment.map(renderUriTemplateFragment).getOrElse("")
  }

  private def renderUriTemplateParam(isFirst: Boolean, p: UriTemplate.PlaceHolder) = {
    val prfx = if (isFirst) "{?" else "{&"
    val sfx = "}"
    s"$prfx${p.name}$sfx"
  }

  private def renderUriTemplateParam(isFirst: Boolean, par: UriTemplate.Param): String = {
    par.value.fold(
      (pl: UriTemplate.PlaceHolder) => renderUriTemplateParam(isFirst, pl),
      {
        case (name, value) =>
          val prfx = if (isFirst) "?" else "&"
          val parName = name.value.fold(
            pl => s"{${pl.name}}",
            parName => paramEscape(parName))
          val parValue = value.value.fold(
            pl => s"={${pl.name}}",
            v => v.map(v => "=" + paramEscape(v)).getOrElse(""))
          prfx + parName + parValue
      })
  }

  private def renderUriTemplateSegment(sg: UriTemplate.Segment): String = {
    sg.value.fold(
      p => s"{${p.name}}",
      s => segmentEscape(s))
  }

  private def renderUriTemplateFragment(f: UriTemplate.Fragment) =
    f.value.fold(pl => s"{#${pl.name}}", frag => "#" + frag)

}