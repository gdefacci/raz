package org.obl.raz
package ext

class ResourceHolder {

  class BaseResource(protected val prefix: Seq[String]) extends RelativePath[SegmentPosition, SegmentPosition](PathSg(prefix), Nil, None) with PathSegmentAdder[SegmentPosition] {
    def this(prefix: String) = this(Seq(prefix))
    def segmentAdderSelf = this
  }  
}


class AbsoluteResourceHolder(host:PathBase) {

  class BaseResource(protected val prefix: Seq[String]) extends AbsolutePath[SegmentPosition](host, PathSg(prefix), Nil, None) with PathSegmentAdder[BasePosition] {
    def this(prefix: String) = this(Seq(prefix))
    def segmentAdderSelf = this
  }
}

