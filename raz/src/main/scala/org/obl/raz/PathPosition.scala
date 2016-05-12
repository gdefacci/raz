package org.obl.raz

sealed trait PathPosition

object PathPosition {
  
  sealed abstract class Absolute extends PathPosition
  sealed abstract class Segment extends Absolute  
  sealed abstract class Param extends Segment
  sealed abstract class Fragment extends Param
  
}

sealed trait PathAppender[S <: PathPosition, E <: PathPosition]

object PathAppender {
  
  import PathPosition._
  
  private def create[S <: PathPosition, E <: PathPosition]:PathAppender[S,E] = new PathAppender[S,E] {}
  
  implicit val segmentAddSegment = create[Segment, Segment] 
  implicit val segmentAddParam = create[Segment, Param] 
  implicit val segmentAddFragment = create[Segment, Fragment] 
  implicit val paramAddParam = create[Param, Param] 
  implicit val paramAddFragment = create[Param, Fragment] 
  
}

