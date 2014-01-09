package org.obl.raz

import org.junit._

class MapperTest {

  case class Cl1(a:Int, b:String, c:Boolean)
  
  import PathFs._
  import TestHelper._
  
  @Test
  def testRootPath = {
    val u1 = Raz / "a" / pathVar[Int] / "b" / pathVar[String] / pathVar[Boolean]
    
    println(u1.toF.apply(10, "abba", true))
    println(u1.toF.apply(10, "abba", true).render)
    println(u1.matcher.apply( u1.toF.apply(10, "abba", true) ))
    var sc = Converter(Cl1.tupled, Cl1.unapply)
    
    val u2 = u1.mapper.mapTo(sc) 

    check3(u1, 10, "abba", true)
    check1(u2, Cl1(10, "abba", true))
    
  }
  
  @Test
  def testRootUri = {
    val u1 = Raz / "bbb" && ("a", "bb") && paramValueVar[Int]("par1") && ("b", "bbb") && paramValueVar[String]("par2") && paramValueVar[Boolean]("par3")
    
    var sc = Converter(Cl1.tupled, Cl1.unapply)
    
    val u2 = u1.mapper.mapTo(sc) 
    
    check3(u1, 10, "abba", true)
    check1(u2, Cl1(10, "abba", true))
  }

  @Test
  def testRootParams = {
    val u1 = Raz && ("a", "bb") && paramValueVar[Int]("par1") && ("b", "bbb") && paramValueVar[String]("par2") && paramValueVar[Boolean]("par3")
    
    var sc = Converter(Cl1.tupled, Cl1.unapply)
    
    val u2 = u1.mapper.mapTo(sc) 
    
    check3(u1, 10, "abba", true)
    check1(u2, Cl1(10, "abba", true))
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
    
     val postalAdressParams = Raz && 
   		paramValueVar[String](shippingForm.firstName) && 
   		paramValueVar[String](shippingForm.lastName) &&
   		paramValueVar[String](shippingForm.streetType) &&
   		paramValueVar[String](shippingForm.street) &&
   		paramValueVar[String](shippingForm.streetNumber) &&
   		paramValueVar[Int](shippingForm.city) &&
   		paramValueVar[String](shippingForm.province) &&
   		paramValueVar[String](shippingForm.postalCode) &&
   		paramValueVar[String](shippingForm.country)
  
   		
   	val po = PostalAddress("firstName", "lastName", "stType", "street", "numb", 12, "HO", "3456", "AN")	
   	val mu = postalAdressParams.mapper.mapTo(Converter(PostalAddress.tupled, PostalAddress.unapply))	
   	val mupth:Path = mu.toF.apply(po)	
   	
   	
   	Assert.assertEquals( 
   	    "?first-name=firstName&last-name=lastName&street-type=stType&street=street&street-number=numb&city=12&province=HO&postal-code=3456&country=AN",
   	    mupth.render )
   	    
   	    
   	mu.matcher.apply( mupth  ) match  {
       case Some(PathMatchResult(po1, _)) if po1 == po => ()
       case x => Assert.fail(x.toString)
     }    
  }
  
  
}