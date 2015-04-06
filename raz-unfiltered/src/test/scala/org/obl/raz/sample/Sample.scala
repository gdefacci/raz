package org.obl.raz.sample

import unfiltered.filter.Plan
import unfiltered.request.GET

object Sample extends App {

  import org.obl.raz._
  import PathConverter._

  val state = RelativePath / "countries" / Segment.string / "states" / Segment.string 

  assert("/countries/it/states/mi" == state("it", "mi").render)

  assert("/countries/{country}/states/{state}" == state.toUriTemplate("country", "state").render)
  
  val street = RelativePath / "cities" / Segment.int && Param.string("street") && Param.string("number")
  
  assert("/cities/123?street=Baker+street&number=12a" == street(123, "Baker street", "12a").render)
  println(street.toUriTemplate("city-id", "street", "number").render)
  assert("/cities/{city-id}?street={street}&number={number}" == street.toUriTemplate("city-id", "street", "number").render )

  /**
   * Absolute url
   */
  
  assert("http://mypage.com/countries/it/states/mi" == state.at(HTTP("mypage.com")).apply("it", "mi").render)
  assert("http://mypage.com/countries/{country}/states/{state}" == state.at(HTTP("mypage.com")).toUriTemplate("country", "state").render)
  
  /**
   * Concatenation
   */
  
  val fullAdrs = state.concat( street )

  assert("/countries/it/states/mi/cities/123?street=Baker+street&number=12a" == fullAdrs("it", "mi", 123, "Baker street", "12a").render)

  assert("/countries/{country}/states/{state}/cities/{city-id}?street={street}&number={street-number}" == fullAdrs.toUriTemplate("country", "state", "city-id", "street", "street-number").render)

  /**
   * Mapping
   */
  
  case class CityStreetNumber(cityId:Int, street:String, number:String)
  
  val mstreet = street.caseMap(CityStreetNumber.tupled, CityStreetNumber.unapply)
  
  assert("/cities/123?street=Baker+street&number=12a" == mstreet(CityStreetNumber(123, "Baker street", "12a")).render)
  
  /**
   * Create a Unfiltered Intent 
   */
  
  val Address = fullAdrs
  
//    val myIntent:Plan.Intent = {
//      case GET(Address(country, state, cityId, street, streetNumber)) => unfiltered.response.Ok
//    }
    
 
  println("done")

}