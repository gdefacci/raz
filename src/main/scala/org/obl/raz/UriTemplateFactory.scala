package org.obl.raz

object UriTemplateFactory extends UTFactory {

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
      case x if (x == ExpansionKind.stringExpansion) => (name:String) => sum( Path(PathSg(stringExpansion(name).toSeq)), pathf.suffix) 
      case x if (x == ExpansionKind.reservedExpansion) => (name:String) => sum( Path(PathSg(reservedExpansion(name).toSeq)), pathf.suffix) 
      case ExpansionKind.ParamValueStringExpansion(nm) => (name:String) => sum( Path(QParamSg(nm, stringExpansion(name))) ,pathf.suffix)
      case ExpansionKind.ParamValueReservedExpansion(nm) => (name:String) => sum( Path(QParamSg(nm, reservedExpansion(name))) ,pathf.suffix)
      case ExpansionKind.formStyleQueryExpansion => {
        val sgrenderer: (Boolean, (String,Option[String])) => String = {(isFirst, nameValue) =>
          if (isFirst) formStyleExpansion(nameValue._1).get
          else formStyleContinuation(nameValue._1).get
        }
        (name:String) => sum( Path( QParamSg(name, None, sgrenderer) ) ,pathf.suffix)
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