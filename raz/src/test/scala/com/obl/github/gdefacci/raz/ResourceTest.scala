package com.github.gdefacci.raz

import ext._

import org.scalatest.FunSuite
import scalaz.{ -\/, \/, \/- }
import sample.{ SampleResources, PlayerId }
import shapeless._

class ResourceTest extends FunSuite {

  test("WsResource") {

    val playerIdSegment = PathConverter.Segment.long.map(PlayerId(_)).contramap((id: PlayerId) => id.id)

    object WsPlayerByIResource extends WebSocketResource(WS("www.site.com"), Path / "app", Path / "ws") {

      lazy val byId = this / "players" / playerIdSegment

    }

    val wsPlayerById = WsPlayerByIResource.byId.pathConverter

    assert("ws://www.site.com/app/ws/players/12" == wsPlayerById.encode(PlayerId(12)).render)
    assert("ws://www.site.com/app/ws/players/{id}" == wsPlayerById.fullPath.encodeUriTemplate("id").render)

    assert(\/-(PlayerId(12)) == wsPlayerById.decodeFull(Path / "app" / "ws" / "players" / "12"))

    val p1 = WS("www.site.com") / "app" / "ws" / "players" / "12"
    assert(\/-(PlayerId(12)) == wsPlayerById.fullPath.decodeFull(p1))

    val p2 = Path / "app" / "ws" / "players" / "12"
    assert(\/-(PlayerId(12)) == wsPlayerById.fullPath.decoderAt(WS("www.site.com")).decodeFull(p2))

    assert("/ws/players/{id}" == wsPlayerById.encodeUriTemplate("id").render)

  }

  test("web socket playerid") {

    val playerIdSegment = PathConverter.Segment.long.map(PlayerId(_)).contramap((id: PlayerId) => id.id)

    val wsPlayerByIdPath = WS("www.site.com") / "app" / "ws" / "players" / playerIdSegment

    val wsPlayerById = wsPlayerByIdPath.pathConverter.decoderAt(WS("www.site.com")).uriTemplateEncoderAt(WS("www.site.com") / "app")

    assert("ws://www.site.com/app/ws/players/12" == wsPlayerById.encode(PlayerId(12)).render)
    assert("ws://www.site.com/app/ws/players/{id}" == wsPlayerById.fullPath.encodeUriTemplate("id").render)

    assert(\/-(PlayerId(12)) == wsPlayerById.decodeFull(Path / "app" / "ws" / "players" / "12"))

    val p1 = WS("www.site.com") / "app" / "ws" / "players" / "12"
    assert(\/-(PlayerId(12)) == wsPlayerById.fullPath.decodeFull(p1))

    val p2 = Path / "app" / "ws" / "players" / "12"
    assert(\/-(PlayerId(12)) == wsPlayerById.fullPath.decoderAt(WS("www.site.com")).decodeFull(p2))

    assert("/ws/players/{id}" == wsPlayerById.encodeUriTemplate("id").render)
  }

  test("playerid") {

    val playerIdSegment = PathConverter.Segment.long.map(PlayerId(_)).contramap((id: PlayerId) => id.id)

    val wsPlayerByIdPath = WS("www.site.com") / "app" / "players" / playerIdSegment

    val wsPlayerById = wsPlayerByIdPath.pathConverter.decoderAt(WS("www.site.com") / "app" / "players")

    assert("ws://www.site.com/app/players/12" == wsPlayerById.encode(PlayerId(12)).render)
    assert("ws://www.site.com/app/players/{id}" == wsPlayerById.fullPath.encodeUriTemplate("id").render)

    assert(\/-(PlayerId(12)) == wsPlayerById.decodeFull(Path / "12"))

    val p1 = WS("www.site.com") / "app" / "players" / "12"
    assert(\/-(PlayerId(12)) == wsPlayerById.fullPath.decodeFull(p1))

    assert("ws://www.site.com/app/players/{id}" == wsPlayerById.encodeUriTemplate("id").render)
  }

}