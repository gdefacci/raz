package org.obl.raz.sample

import org.obl.raz.Api._
import org.obl.raz.UriTemplate

trait PlayerRoutes {
  def Players: PathMatchDecoder
  def PlayerLogin: PathMatchDecoder
  def PlayerById: PathCodec.Symmetric[PlayerId]
}

trait PlayerWebSocketRoutes {
  def PlayerById: PathCodec.Symmetric[PlayerId]
  def playerByIdUriTemplate: UriTemplate
}

trait GameRoutes {
  def Games: PathMatchDecoder
  def GameById: PathCodec.Symmetric[GameId]
  def Player: PathCodec.Symmetric[(GameId, PlayerId)]
  def Team: PathCodec.Symmetric[(GameId, String)]
}

trait CompetitionRoutes {
  def Competitions: PathMatchDecoder
  def CompetitionById: PathCodec.Symmetric[CompetitionId]
  def PlayerCompetitionById: PathCodec.Symmetric[(CompetitionId, PlayerId)]
  def AcceptCompetition: PathCodec.Symmetric[(CompetitionId, PlayerId)]
  def DeclineCompetition: PathCodec.Symmetric[(CompetitionId, PlayerId)]
  def CreateCompetition: PathCodec.Symmetric[PlayerId]
}

class AppRoutes(sampleResources:SampleResources) {
  
  lazy val playerRoutes = new PlayerRoutes {
    lazy val Players = sampleResources.Players.pathMatchDecoder
    lazy val PlayerLogin = sampleResources.Players.login.pathMatchDecoder
    
    
    lazy val PlayerById = sampleResources.Players.byId.pathCodec
  }
  
  lazy val playerWebSocketRoutes = new PlayerWebSocketRoutes {
    lazy val PlayerById = sampleResources.WebSockets.Players.byId.pathConverter
    lazy val playerByIdUriTemplate: UriTemplate = this.PlayerById.encodeUriTemplate("playerId")
  }
  
  lazy val gameRoutes = new GameRoutes {
    lazy val Games = sampleResources.Games.pathMatchDecoder
    lazy val GameById = sampleResources.Games.byId.pathCodec
    lazy val Player = sampleResources.Games.player.pathCodec
    lazy val Team = sampleResources.Games.team.pathCodec
  }
  
  lazy val competitionRoutes  = new CompetitionRoutes {
    lazy val Competitions = sampleResources.Competitions.pathMatchDecoder
    lazy val CompetitionById = sampleResources.Competitions.byId.pathCodec
    lazy val PlayerCompetitionById = sampleResources.Competitions.player.pathCodec
    lazy val AcceptCompetition = sampleResources.Competitions.accept.pathCodec
    lazy val DeclineCompetition = sampleResources.Competitions.decline.pathCodec
    lazy val CreateCompetition = sampleResources.Competitions.create.pathCodec
  }
  
}