package org.obl.raz

object GenMatchers extends App {
  
  def gen(i:Int) = {
    assert(i > 1)
    
    val i1 = i - 1
	  val prevTyps = 1.to(i-1).map(i => s"T$i").mkString(",")
    val typs = 1.to(i).map(i => s"T$i").mkString(",")
    val lt = s"T$i"
    val pars =  
      if (i1 == 1) "pmr.value"
      else         1.to(i-1).map( i => s"pmr.value._$i" ).mkString(", ")
    
s"""
  implicit def matcher$i[$typs] = apply[DecHPath$i[$typs], ($typs)]( { h =>
    matcher$i1[$prevTyps].decoder(h.head).flatMap { pmr =>
      h.value.decode(pmr.rest).map( res => res.mapValue { v:$lt => ($pars, v) })
    }
  })"""
    
  }
  
  println( 2.to(22).map(gen).mkString("\n") )
  
  
}