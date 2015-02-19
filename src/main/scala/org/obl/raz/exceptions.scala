package org.obl.raz

object exceptions {

  case object EndOfPathException extends Exception

  def invalidExpectationMsg(part: String, actual: Path) = s"expecting $part, but the ${actual.render} has been encoutered"

  case class PathExpectationException(expected: Path, actual: Path) extends Exception(invalidExpectationMsg(s"path ${expected.render}", actual))

  sealed trait PathPart

  object PathPart {
    case object SegmentPart extends PathPart
    case object ParamsPart extends PathPart
    case object FragmentPart extends PathPart
    case class ParamValue(name: String) extends PathPart
    case class Param(description: String) extends PathPart
  }

  case class MissingPathPartException(part: PathPart, actual: Path) extends Exception(s"part $part is missing")

  private def chooseMsg(causes:Seq[Throwable]) = s"choose decode error: ${causes.map(_.getMessage).mkString("\n")}"
  
  case class ChooseDecodeException(causes:Seq[Throwable]) extends Exception(chooseMsg(causes))
}