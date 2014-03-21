Raz
---

Raz is a type safe scala DSL to create and match urls.
The following program illustrates few features:

    val state = Raz / "countries" / pathVar[String] / "states" / pathVar[String] 

    assert("/countries/it/states/mi" == state("it", "mi").render)

    assert("/countries/{country}/states/{state}" == state.toUriTemplate("country", "state"))
    
    val street = Raz / "cities" / pathVar[Int] && paramValueVar[String]("street") && paramValueVar[String]("number")
    
    assert("/cities/123?street=Baker+street&number=12a" == street(123, "Baker street", "12a").render)
    assert("/cities/{city-id}?street={street}&number={number}" == street.toUriTemplate("city-id", "street", "number"))

    /**
     * Absolute url
     */

    assert("http://mypage.com/countries/it/states/mi" == state("it", "mi").at("http://mypage.com").render)
    assert("http://mypage.com/countries/it/states/mi" == state.at("http://mypage.com").apply("it", "mi").render)
    assert("http://mypage.com/countries/{country}/states/{state}" == state.at("http://mypage.com").toUriTemplate("country", "state"))
    
    /**
     * Concatenation
     */
    
    var fullAdrs = state.concat( street )

    assert("/countries/it/states/mi/cities/123?street=Baker+street&number=12a" == fullAdrs("it", "mi", 123, "Baker street", "12a").render)

    assert("/countries/{country}/states/{state}/cities/{city-id}?street={street}&number={street-number}" == fullAdrs.toUriTemplate("country", "state", "city-id", "street", "street-number"))

    /**
     * Mapping
     */
    
    case class CityStreetNumber(cityId:Int, street:String, number:String)
    
    val mstreet = street.mapTo(Converter(CityStreetNumber.tupled, CityStreetNumber.unapply))
    
    assert("/cities/123?street=Baker+street&number=12a" == mstreet(CityStreetNumber(123, "Baker street", "12a")).render)
    
    /**
     * toUriTemplate is not avaiable on mstreet
     * 
     * val myUt = mstreet.toUriTemplate // DOES NOT COMPILE 
     */
    
    /**
     * Match path
     */

    val pth1:Path = mstreet(CityStreetNumber(223, "Baker street", "112a"))   && ("extra-param", "other value") 
    
    mstreet.matchPath(pth1) match {
      case Some(PathMatchResult(CityStreetNumber(223, "Baker street", "112a"), rest)) => {
        assert(rest.path.isEmpty)
        assert(rest.params.length == 1)
        rest.params.head match {
          case QParamSg(name, v) => {
            assert(name == "extra-param")
            assert(v == Some("other value"))
          }
        } 
      }
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
 	    