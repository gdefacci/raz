package org.obl.raz

import java.net.URLDecoder

object ReplaceTripletsTest extends App {
  
  def check(s:String) = {
    assert(URLDecoder.decode(s, "UTF-8") == DecodeUtils.replacePercentTriplets(s))
  }
  
  def checkFail(s:String) = {
    var failed = false
    try {
      DecodeUtils.replacePercentTriplets(s)
      failed = true
    } catch {
      case e:Throwable => ()
    }
    assert(!failed, "expecting a falure")
  }

  
  check("akddk")
  check("ak%34ddk")
  check("ak%34d%23dk")
  check("ak%34%943%23%23d%23dk")
  
  checkFail("ak%34%943%23%23d%23dk%5")
  checkFail("ak%34%943%23%23d%-23dk%5")
  checkFail("ak%a34%943%23%23d%-23dk%5")
}