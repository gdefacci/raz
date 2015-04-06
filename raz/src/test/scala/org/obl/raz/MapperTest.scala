package org.obl.raz

import org.junit._
import scalaz.\/-

class MapperTest {
  
  import PathConverter._

  case class Cl1(a:Int, b:String, c:Boolean)
  
  import TestHelper._
  
  @Test
  def testRootPath = {
    val u1 = RelativePath / "a" / Segment.int / "b" / Segment.string / Segment.boolean
    
    println(u1(10, "abba", true))
    println(u1(10, "abba", true).render)
    println(u1.decodeFull( u1(10, "abba", true) ))
    
//    var sc = Converter.tryConverter(Cl1.tupled, Cl1.unapply)
    
    val u2 = u1.caseMap(Cl1.tupled, Cl1.unapply) 

    check(u1, (10, "abba", true))
    check(u2, Cl1(10, "abba", true))
    
  }
  
  @Test
  def testRootUri = {
    val u1 = RelativePath / "bbb" && ("a", "bb") && Param.int("par1") && ("b", "bbb") && Param.string("par2") && Param.boolean("par3")
    
//    var sc = Converter.tryConverter(Cl1.tupled, Cl1.unapply)
    
    val u2 = u1.caseMap(Cl1.tupled, Cl1.unapply)
    
    check(u1, (10, "abba", true))
    check(u2, Cl1(10, "abba", true))
  }

  @Test
  def testRootParams = {
    val u1 = RelativePath && ("a", "bb") && Param.int("par1") && ("b", "bbb") && Param.string("par2") && Param.boolean("par3")
    
//    var sc = Converter.tryConverter(Cl1.tupled, Cl1.unapply)
    
    val u2 = u1.caseMap(Cl1.tupled, Cl1.unapply) 
    
    check(u1, (10, "abba", true))
    check(RelativePath && u2, Cl1(10, "abba", true))
  }

  
  object shippingForm {				
    val firstName = "first-name"
    val lastName = "last-name"
    val email = "email"
    val country = "country"
    val streetType = "street-type"
    val street = "street"
    val streetNumber = "street-number"
    val city = "city"
    val province = "province"
    val postalCode = "postal-code"
  }
  
  case class PostalAddress(
      firstName:String, 
      lastName:String, 
      streetType:String, 
      street:String, 
      streetNumber:String, 
      city:Int, 
      province:String, 
      postalCode:String, 
      country:String) 

  
  @Test
  def testPostalAddress = {
    
     val postalAdressParams = RelativePath && 
   		Param.string(shippingForm.firstName) && 
   		Param.string(shippingForm.lastName) &&
   		Param.string(shippingForm.streetType) &&
   		Param.string(shippingForm.street) &&
   		Param.string(shippingForm.streetNumber) &&
   		Param.int(shippingForm.city) &&
   		Param.string(shippingForm.province) &&
   		Param.string(shippingForm.postalCode) &&
   		Param.string(shippingForm.country)
  
    println("=================")
    println(postalAdressParams("firstName", "lastName", "stType", "street", "numb", 12, "HO", "3456", "AN").render)
   		
   		
   	val po = PostalAddress("firstName", "lastName", "stType", "street", "numb", 12, "HO", "3456", "AN")	
   	val mu = postalAdressParams.caseMap(PostalAddress.tupled, PostalAddress.unapply)	
   	val mupth:Path = mu(po)	
   	
   	println("==============")
   	println(mupth.render )
   	Assert.assertEquals( 
   	    "/?first-name=firstName&last-name=lastName&street-type=stType&street=street&street-number=numb&city=12&province=HO&postal-code=3456&country=AN",
   	    mupth.render )
   	    
   	    
   	mu.decode( mupth  ) match  {
       case \/-(PathMatchResult(po1, _)) if po1 == po => ()
       case x => Assert.fail(x.toString)
     }    
  }
  
  
}