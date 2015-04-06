package org.obl.raz

object UriTemplateUtils {

  def mergeAll(pths:Seq[UriTemplate]):UriTemplate = {
    pths.headOption match {
      case None => UriTemplate.empty
      case Some(phd) => {
        var p = collection.mutable.Buffer.empty[UriTemplatePathSegment] ++ phd.path
        val q = collection.mutable.Buffer.empty[UriTemplateParam] ++ phd.params
        val paths = pths.tail
        paths.foreach { pth =>
          p ++= pth.path
          q ++= pth.params
        }
        UriTemplate(phd.base,p, q, paths.lastOption.flatMap(_.fragment))   
      }
    }
  }
  
  def merge(pths:UriTemplate*):UriTemplate = {
    mergeAll(pths)
  }

}