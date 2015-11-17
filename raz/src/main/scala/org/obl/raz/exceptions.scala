package org.obl.raz

object exceptions {

  case object EndOfPathException extends Exception

  def invalidExpectationMsg(part: String, actual: Path) = s"expecting $part, but the ${actual.render} has been encoutered"

  final case class PathExpectationException(expected: Path, actual: Path) extends Exception(invalidExpectationMsg(s"path ${expected.render}", actual))

  sealed trait PathPart

  object PathPart {
    case object SegmentPart extends PathPart
    case object ParamsPart extends PathPart
    case object FragmentPart extends PathPart
    final case class ParamValue(name: String) extends PathPart
    final case class Param(description: String) extends PathPart
  }

  final case class MissingPathPartException(part: PathPart, actual: Path) extends Exception(s"part $part is missing")

  
}