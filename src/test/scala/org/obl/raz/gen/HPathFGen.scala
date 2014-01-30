package org.obl.raz.gen.exp

object HPathFGen {

  def gen(n: Int): String = {
    val ts = 1.to(n).map(i => s"T$i").mkString(",")
    val rt = if (n == 1) "T1" else "(" + ts + ")"

    val typ = 
      if (n==1) "T1"
      else "("+ts+")"
    
    val mergePars = (1.to(n).map { i =>
      val headNumb = n - i
      if (headNumb > 0) {
        val headAccessor = 1.to(headNumb).map(i => "head").mkString(".")

        s"h.$headAccessor.value.apply(t1._$i)"
      } else {
        if (n > 1) s"h.value.apply(t1._$i)"
        else s"h.value.apply(t1)"
      }
    }).mkString(", ")

    val firstParHeadNumb = 1.to(n).map(i => "head").mkString(".")

  s"""
  implicit def toHPathF$n[R <: RelativePathAspect,A <: CanAddAspect,P <: CanHavePrefixAspect,$ts] =
    apply[HPath$n[R,A,P,$ts], $typ,BasePath[R,A,P]](h => t1 => sum[R,A,P](h.$firstParHeadNumb.path,  $mergePars))  
  """
  }

  def main(args: Array[String]): Unit = {

    println(1.to(22).map(gen).mkString("\n"))
  }

}