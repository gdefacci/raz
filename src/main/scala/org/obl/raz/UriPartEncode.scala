package org.obl.raz

private [raz] object UriPartEncode {
  
  private val hexDigit = Array[Char]('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');

  private def escape(ch:Char):String = "%" + hexDigit(((ch >> 4) & 0xF)) + hexDigit(((ch >> 0) & 0xF))

  val alphaNum = ('0' to '9').toSet ++ ('A' to 'Z') ++ ('a' to 'z')
  var uriTemplateExtraUnescaped = Set('{', '}')
  
  val pathUnescaped:Set[Char] =  alphaNum ++ Set[Char]('.', '-', '~', '_', '@', ':' , '!', '$', '&', ''', '(', ')', '*', '+', ',', ';', '=')
  val pathUriTemplateUnescaped:Set[Char] =  pathUnescaped ++ uriTemplateExtraUnescaped

  val paramUnescaped = alphaNum ++ Set('.', '-', '*', '_') 
  val paramUriTemplateUnescaped = paramUnescaped ++ uriTemplateExtraUnescaped

  val fragmentUnescaped = alphaNum ++ Set('.', '-', '~', '_', '@', ':', '!', '$', '&', '(', ')', '*', '+', ',', ';', '=', '\'', '/', '?')

  def encode(unaltered:Set[Char])(str:String):String = {
    val sb = new StringBuilder()
    str.foreach { ch =>
      if (unaltered.contains(ch)) sb.append(ch)
      else sb.append(escape(ch)) 
    }
    sb.toString
  }

}