package org.obl.raz

object UriChars {

  val alphaNum = ('0' to '9').toSet ++ ('A' to 'Z').toSet ++ ('a' to 'z').toSet  
  val reserved = Set(';', '/', '?', ':', '@', '&', '=', '+', '$', ',')
  
  private val hexDigit = Array[Char]('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');
  
  def escape(ch:Char):String = "%" + hexDigit(((ch >> 4) & 0xF)) + hexDigit(((ch >> 0) & 0xF))
  
  val mark = Set('-', '_', '.', '!', '~', '*', ''', '(', ')')

  val unreserved  = alphaNum ++ mark
  
  val pchar = unreserved ++ Set(':', '@', '&', '=', '+', '$', ',')
  
  val unwise      = Set('{','}','|','\\','^','[',']')
  val delims 	  = Set('<', '>', '#', '%', '<', '>')
  
  val uric        = reserved ++ unreserved
  
  def toEscape(ch:Char) = {
    (ch.toInt <= 32) || (ch.toInt >= 127) || unwise.contains(ch) || delims.contains(ch)  
  }
  
  def pathChar(ch:Char):String = 
    if (pchar(ch)) ch.toString
    else escape(ch)
  
  def paramChar(ch:Char):String = 
    if (uric(ch)) ch.toString
    else escape(ch)  
    
 def path(sg:String):String = sg.flatMap(pathChar)
  
 def paramPart(p:String):String = p.flatMap(paramChar)
  
}