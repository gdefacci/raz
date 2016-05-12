package org.obl.raz

import org.scalatest.FunSuite
import scalaz.{ \/- }
import sample._

class AppRoutesTest extends FunSuite {

  val appRoutes = new AppRoutes(new SampleResources(HTTP, Authority("localhost", 8080), Path / "app"))

  val player1 = PlayerId(1)
  val game1 = GameId(2)
  val competition1 = CompetitionId(3)

  test("playerWebSocketRoutes") {

    val rts = appRoutes.playerWebSocketRoutes

    assert(rts.PlayerById.encode(player1).render == "ws://localhost:8080/app/ws/players/1")

    assert(rts.PlayerById.decode(Path / "app" / "ws" / "players" / "1").map(_.value) == \/-(player1))

    assert("/ws/players/{playerId}" == appRoutes.playerWebSocketRoutes.playerByIdUriTemplate.render)

  }

  test("players / byId") {

    val rts = appRoutes.playerRoutes

    assert(rts.PlayerById.encode(player1).render == "http://localhost:8080/app/players/1")

    assert(rts.PlayerById.decode(Path / "1").map(_.value) == \/-(player1))

    val p1 = Path(Some(HTTP), Some(Authority("localhost",8080)), List("app", "players", "1"), Nil, None)
    
    val dec1 = (HTTP("localhost",8080) / "app" / "players" / PathConverter.Segment.string).pathConverter

    assert(dec1.decodeFull(p1) == \/-("1"))
    
    val r1 = rts.PlayerById.fullPath.decodeFull(p1)
    assert(rts.PlayerById.fullPath.decodeFull(p1) == \/-(player1))

  }

  test("playerLogin") {

    val rts = appRoutes.playerRoutes

    assert(rts.PlayerLogin.path.render == "http://localhost:8080/app/players/login")

    assert(rts.PlayerLogin.decodeFull(Path / "login") == \/-(Path / "login"))
  }

  test("players") {

    val rts = appRoutes.playerRoutes

    assert(rts.Players.path.render == "http://localhost:8080/app/players")

    assert(rts.Players.decodeFull(Path) == \/-(Path))
  }

  test("games / byId") {

    val rts = appRoutes.gameRoutes

    assert(rts.GameById.encode(game1).render == "http://localhost:8080/app/games/2")

    assert(rts.GameById.decode(Path / "2").map(_.value) == \/-(game1))
  }

  test("games") {

    val rts = appRoutes.gameRoutes

    assert(rts.Games.path.render == "http://localhost:8080/app/games")

    assert(rts.Games.decodeFull(Path) == \/-(Path))
  }

  test("games / player") {

    val rts = appRoutes.gameRoutes

    assert(rts.Player.encode(game1, player1).render == "http://localhost:8080/app/games/2/player/1")

    assert(rts.Player.decode(Path / "2" / "player" / "1").map(_.value) == \/-(game1 -> player1))
  }

  test("games / team") {

    val rts = appRoutes.gameRoutes

    assert(rts.Team.encode(game1, "team-a").render == "http://localhost:8080/app/games/2/team/team-a")

    assert(rts.Team.decode(Path / "2" / "team" / "team-a").map(_.value) == \/-(game1 -> "team-a"))
  }

  test("competitions") {

    val rts = appRoutes.competitionRoutes

    assert(rts.Competitions.path.render == "http://localhost:8080/app/competitions")

    assert(rts.Competitions.decodeFull(Path) == \/-(Path))
  }

  test("competitons / accept") {

    val rts = appRoutes.competitionRoutes

    assert(rts.AcceptCompetition.encode(competition1, player1).render == "http://localhost:8080/app/competitions/3/player/1/accept")

    assert(rts.AcceptCompetition.decode(Path / "3" / "player" / "1" / "accept").map(_.value) == \/-(competition1 -> player1))
  }
  
  test("competitons / decline") {

    val rts = appRoutes.competitionRoutes

    assert(rts.DeclineCompetition.encode(competition1, player1).render == "http://localhost:8080/app/competitions/3/player/1/decline")

    assert(rts.DeclineCompetition.decode(Path / "3" / "player" / "1" / "decline").map(_.value) == \/-(competition1 -> player1))
  }
  
  test("competitons / create") {

    val rts = appRoutes.competitionRoutes

    assert(rts.CreateCompetition.encode(player1).render == "http://localhost:8080/app/competitions/player/1")

    assert(rts.CreateCompetition.decode(Path / "player" / "1").map(_.value) == \/-(player1))
  }

  test("competitons / byId") {

    val rts = appRoutes.competitionRoutes

    assert(rts.CompetitionById.encode(competition1).render == "http://localhost:8080/app/competitions/3")

    assert(rts.CompetitionById.decode(Path / "3").map(_.value) == \/-(competition1))
  }
  
 test("competitons / playerCompetition") {

    val rts = appRoutes.competitionRoutes

    assert(rts.PlayerCompetitionById.encode(competition1, player1).render == "http://localhost:8080/app/competitions/3/player/1")

    assert(rts.PlayerCompetitionById.decode(Path / "3" / "player" / "1").map(_.value) == \/-(competition1 -> player1))
  }

}