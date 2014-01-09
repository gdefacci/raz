package org.obl.raz

object ExpansionKind extends Enumeration {
  
  case class ParamValueStringExpansion(paramName:String) extends Val
  case class ParamValueReservedExpansion(paramName:String) extends Val
  
  val stringExpansion, reservedExpansion, formStyleQueryExpansion = Value
  
}
