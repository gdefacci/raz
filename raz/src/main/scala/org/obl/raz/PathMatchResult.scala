package org.obl.raz

case class PathMatchResult[+T, +R](value:T, rest:R) {
  def mapValue[P,T1 >: T](f:T1 => P):PathMatchResult[P,R] = PathMatchResult[P,R](f(value), rest)
}