Raz
---

Raz is a type safe scala DSL to create and match urls.
The following program illustrates few features:

    val state = Raz / "countries" / pathVar[String] / "states" / pathVar[String] 

    assert("/countries/it/states/mi" == state.toF.apply("it", "mi").render)

    assert("/countries/{country}/states/{state}" == state.toUriTemplate.toF.apply("country", "state").render)
    
    val street = Raz / "cities" / pathVar[Int] && paramValueVar[String]("street") && paramValueVar[String]("number")
    
    assert("/cities/123?street=Baker street&number=12a" == street.toF.apply(123, "Baker street", "12a").render)
    assert("/cities/{city-id}?street={street}&number={number}" == street.toUriTemplate.toF.apply("city-id", "street", "number").render)

    /**
     * Absolute url
     */

    assert("http://mypage.com/countries/it/states/mi" == state.toF.apply("it", "mi").at("http://mypage.com").render)
    assert("http://mypage.com/countries/{country}/states/{state}" == state.toUriTemplate.toF.apply("country", "state").at("http://mypage.com").render)
    
    /**
     * Concatenation
     */
    
    var fullAdrs = state.append( street )

    assert("/countries/it/states/mi/cities/123?street=Baker street&number=12a" == fullAdrs.toF.apply("it", "mi", 123, "Baker street", "12a").render)

    assert("/countries/{country}/states/{state}/cities/{city-id}?street={street}&number={street-number}" == fullAdrs.toUriTemplate.toF.apply("country", "state", "city-id", "street", "street-number").render)

    /**
     * Mapping
     */
    
    case class CityStreetNumber(cityId:Int, street:String, number:String)
    
    val mstreet = street.mapper.mapTo(Converter(CityStreetNumber.tupled, CityStreetNumber.unapply))
    
    assert("/cities/123?street=Baker street&number=12a" == mstreet.toF.apply(CityStreetNumber(123, "Baker street", "12a")).render)
    
    /**
     * toUriTemplate is not avaiable on mstreet
     * 
     * val myUt = mstreet.toUriTemplate // DOES NOT COMPILE 
     */
    
    /**
     * Match path
     */

    val pth1:Path = mstreet.toF.apply(CityStreetNumber(223, "Baker street", "112a")) 
    
    mstreet.matcher.apply(pth1) match {
      case Some(PathMatchResult(CityStreetNumber(223, "Baker street", "112a"), _)) => ()
      case x => assert(false, "this should not happen")
    }
    
 
    /**
     * Create a Unfiltered Intent 
     */
    
    val Address = fullAdrs
    
    val myIntent:Plan.Intent = {
      case GET(Address(country, state, cityId, street, streetNumber)) => unfiltered.response.Ok
    }
    
Similar tools
-------------

[Linx](https://github.com/teigen/linx) is a scala library with similar objectives. Compared to Linx, Raz enforse a little more type safety
 	    