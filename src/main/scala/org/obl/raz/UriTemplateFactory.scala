package org.obl.raz

object UriTemplateFactory {

  private def expansion(prefix: String, suffix: String): StringConverter[String] = {
    StringConverter(sg => Some(prefix + sg + suffix), { uts =>
      uts .flatMap { uts =>
      if (uts.length > (prefix.length + suffix.length) && (uts.startsWith(prefix)) && uts.endsWith(suffix)) {
        Some(uts.substring(prefix.length, uts.length - suffix.length))
      } else None
      }
    })
  }

  private val stringExpansion = expansion("{", "}")
  private val reservedExpansion = expansion("{+", "}")
  private val formStyleExpansion = expansion("{?", "*}")
  private val formStyleContinuation = expansion("{&", "*}")
  
  def expansion[T](pathf:PathF[T]):String => Path = {
    import PathHelper._
    
    pathf.expansionKind match {
      case x if (x == ExpansionKind.stringExpansion) => (name:String) => PathHelper.merge( Path(None,PathSg(stringExpansion(name).toSeq), Nil, None ), pathf.suffix) 
      case x if (x == ExpansionKind.reservedExpansion) => (name:String) => PathHelper.merge( Path(None,PathSg(reservedExpansion(name).toSeq), Nil, None), pathf.suffix) 
      case ExpansionKind.ParamValueStringExpansion(nm) => (name:String) => PathHelper.merge( Path(None, PathSg.empty, QParamSg(nm, stringExpansion(name)) :: Nil, None) ,pathf.suffix)
      case ExpansionKind.ParamValueReservedExpansion(nm) => (name:String) => PathHelper.merge( Path(None, PathSg.empty, QParamSg(nm, reservedExpansion(name)) :: Nil, None) ,pathf.suffix)
      case ExpansionKind.formStyleQueryExpansion => {
        val sgrenderer: (Boolean, (String,Option[String])) => String = {(isFirst, nameValue) =>
          if (isFirst) formStyleExpansion(nameValue._1).get
          else formStyleContinuation(nameValue._1).get
        }
        (name:String) => PathHelper.merge( Path(None, PathSg.empty, QParamSg(name, None, sgrenderer) :: Nil, None) ,pathf.suffix)
      }
    }
  }
  
  def utMatcher[T](pathf:PathF[T]):Path => Option[PathMatchResult[String, Path]] = {
    pathf.expansionKind match {
      case x if (x == ExpansionKind.stringExpansion) => PathF.pathMatcher(PathFs.pathVarMatcher(stringExpansion))
      case x if (x == ExpansionKind.reservedExpansion) => PathF.pathMatcher(PathFs.pathVarMatcher(reservedExpansion))
      case ExpansionKind.ParamValueStringExpansion(nm) => PathF.paramMatcher(PathFs.paramVarMatcher( PathFs.paramValueMatcher(nm, stringExpansion)))
      case ExpansionKind.ParamValueReservedExpansion(nm) => PathF.paramMatcher(PathFs.paramVarMatcher( PathFs.paramValueMatcher(nm, reservedExpansion)))
      case ExpansionKind.formStyleQueryExpansion => {
        val mtchr:QParamSg => Option[String] = { 
          case QParamSg(name, None) => Some(name) //formStyleExpansion.unapply(Some(name)).orElse(formStyleContinuation.unapply(Some(name)))
          case _ => None
        }
        PathF.paramMatcher(PathFs.paramVarMatcher( mtchr ))
      }
    }
  }
  
}