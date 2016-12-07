Raz
---

Raz is a type safe scala DSL to create and match urls.
The following example illustrates few features:

```scala
object Sample extends App {

  import com.github.gdefacci.raz._
  import PathConverter.{ Segment, Param, Fragment }

  val state = Path / "countries" / Segment.string / "states" / Segment.string
  val street = Path / "cities" / Segment.int && Param("street").string && Param("number").string

  assert("/countries/it/states/mi" == state.pathConverter.encode("it", "mi").render)
  assert("/countries/{country}/states/{state}" == state.pathConverter.encodeUriTemplate("country", "state").render)

  assert("/cities/123?street=Baker+street&number=12a" == street.pathConverter.encode(123, "Baker street", "12a").render)
  assert("/cities/{city-id}?street={street}&number={number}" == street.pathConverter.encodeUriTemplate("city-id", "street", "number").render)

  val absStreet = HTTP("mypage.com") / "app" / street.pathConverter

  assert("http://mypage.com/app/cities/123?street=Baker+street&number=12a" == absStreet.pathConverter.encode(123, "Baker street", "12a").render)

  val st2 = HTTP("mypage.com") / street.pathConverter
  st2.pathConverter.encode(123, "Baker street", "12a").render

  val fullAdrs = HTTP("mypage.com") / (state ++ street).pathConverter

  assert("http://mypage.com/countries/{country}/states/{state}/cities/{city}?street={street}&number={streetNumber}" ==
    fullAdrs.pathConverter.encodeUriTemplate("country", "state", "city", "street", "streetNumber").render)

  assert("http://mypage.com/countries/it/states/ts/cities/8?street=via+roma&number=12" ==
    fullAdrs.pathConverter.encode("it", "ts", 8, "via roma", "12").render)

  val pathAdrs = HTTP("mypage.com") / "countries" / "it" / "states" / "ts" / "cities" / "8" && ("street", "via roma") && ("number", "12")

  assert(fullAdrs.pathConverter.decodeFull(pathAdrs).toOption == Some(("it", "ts", 8, "via roma", "12")))

  case class Address(country: String, state: String, city: Int, street: String, streetNumber: String)

  val fullAdrs1 = fullAdrs.pathConverter.caseMap(Address.tupled)(Address.unapply)

  assert("http://mypage.com/countries/it/states/ts/cities/8?street=via+roma&number=12" ==
    fullAdrs1.pathConverter.encode(Address("it", "ts", 8, "via roma", "12")).render)

  assert(fullAdrs1.pathConverter.decodeFull(pathAdrs).toOption == Some(Address("it", "ts", 8, "via roma", "12")))
}
```
      
For more samples check

[test folder](https://github.com/gdefacci/raz/tree/master/raz/src/test/scala/org/obl/raz)
[sample project](https://github.com/gdefacci/briscola/tree/master/ddd-briscola-web/src/main/scala/org/obl/briscola/web)
      
Similar tools
-------------

[Linx](https://github.com/teigen/linx) is a scala library with similar objectives. Compared to Linx, Raz enforse a little more type safety
 	    
