package org.obl.raz.sample

import org.obl.raz._
import org.obl.raz.ext._
import PathConverter.{ Segment, Param, Fragment }

case class CompetitionId(id: Long)
case class PlayerId(id: Long)
case class GameId(id: Long)

class SampleResources(scheme: Scheme, authority: Authority, applicationPath: TPath[PathPosition.Segment, PathPosition.Segment]) {

  val playerIdSegment = Segment.long.map(PlayerId(_)).contramap((id: PlayerId) => id.id)

  object WebSockets {

    object Players extends WebSocketResource(WS(authority), applicationPath, Path / "ws") {

      lazy val byId = this / "players" / playerIdSegment

    }

  }

  val prefix = HTTP(authority) append applicationPath

  object Players extends ServletResource(prefix, Path / "players") {

    val byId = this / playerIdSegment

    val login = this / "login"
  }

  object Games extends ServletResource(prefix, Path / "games") {

    private val gameIdSegment = Segment.long.map(GameId(_)).contramap((id: GameId) => id.id)

    val byId = this / gameIdSegment

    val player = byId / "player" / playerIdSegment

    val team = byId / "team" / Segment.string

  }

  object Competitions extends ServletResource(prefix, Path / "competitions") {

    private val competitionIdSegment = Segment.long.map(CompetitionId(_)).contramap((id: CompetitionId) => id.id)

    val byId = this / competitionIdSegment

    val player = byId / "player" / playerIdSegment

    val accept = player / "accept"

    val decline = player / "decline"

    val create = this / "player" / playerIdSegment

  }

  object SiteMap extends ServletResource(prefix, Path / "site-map") {

    val players = Players

  }
}