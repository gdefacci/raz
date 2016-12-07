package com.github.gdefacci.raz

final case class PathExpectationException(expected: Path, actual: Path) extends Exception(s"expecting $expected, but the $actual has been encoutered")
final case object NoMoreSegments extends Exception("expecting more segments")
final case class MissingParameter(name:String) extends Exception(s"cant find parameter $name")
final case class ExpectingSegment(name:String, actual:String) extends Exception(s"expecting segment $name got $actual")
final case object NoFragment extends Exception("expecting fragment")
/*
final case class AuthorityMatchError(expected:Option[SchemeAndAuthority], actual:Option[SchemeAndAuthority]) extends 
  Exception(expected.map(e => s"expecting authority ${e.render} got ${actual.map(_.render)}").getOrElse(s"invalid auhtority :${actual.map(_.render)}"))

*/