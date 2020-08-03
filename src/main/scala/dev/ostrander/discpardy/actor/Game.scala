package dev.ostrander.discpardy.actor

import ackcord.APIMessage.MessageMessage
import ackcord.DiscordClient
import ackcord.data.TextChannel
import ackcord.data.User
import ackcord.data.UserId
import ackcord.syntax.TextChannelSyntax
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import dev.ostrander.discpardy.model
import dev.ostrander.discpardy.model.ClueIndex
import dev.ostrander.discpardy.model.GameProgress
import dev.ostrander.discpardy.model.RoundNumber.Jeopardy
import dev.ostrander.discpardy.model.RoundNumber.DoubleJeopardy
import dev.ostrander.discpardy.model.GameProgress.RoundState
import dev.ostrander.discpardy.model.SecondRound
import scala.io.Source

object Game {
  sealed trait Command {
    def message: MessageMessage
    def user: Option[User] = message.message.authorUser(message.cache.current)
    def isUser: Boolean = user.exists(_.isUser)
    def username: String = user.map(_.username).getOrElse("Unidentifiable User")
  }
  case class StartGame(message: MessageMessage) extends Command
  case class InGame(message: MessageMessage) extends Command

  sealed trait WaitingFor
  case class Selection(maybeUserId: Option[UserId]) extends WaitingFor
  case class Wager(userId: UserId) extends WaitingFor
  case class CorrectAnswer(clueIndex: ClueIndex, incorrect: Set[UserId]) extends WaitingFor

  case class Score(value: Map[UserId, Int])

  val welcomeMessage = Source.fromResource("welcome.txt").toString

  def apply(client: DiscordClient, channel: TextChannel, game: model.Game): Behavior[Command] = {
    def behavior(score: Score, state: RoundState, waitingFor: WaitingFor): Behavior[Command] = {
      val currentRound = state.roundNumber match {
        case Jeopardy => game.firstRound
        case DoubleJeopardy => game.secondRound
      }
      Behaviors.receive {
        case (ctx, sg: StartGame) =>
          if (sg.user.isEmpty) ctx.log.warn(s"Game started by unknown user id")
          client.requests.singleFuture(channel.sendMessage(welcomeMessage.format(sg.username)))
          behavior(score, state, Selection(sg.message.message.authorUserId))
        case (ctx, InGame(message)) => waitingFor match {
          case Selection(maybeUserId) =>
            if (state.isComplete) {
              // move on to next round or complete game
              Behaviors.same
            } else {
              val validSelection = (maybeUserId.isEmpty || maybeUserId == message.message.authorUserId) && message.message.content.contains('$')
              if (!validSelection) Behaviors.same
              else {
                val clueIndex = currentRound.getClueIndex(message.message.content)
                if (state.used(clueIndex)) {
                  // Message channel saying that question has already been chosen
                } else {
                  val clue = game.clue(clueIndex)
                  
                  behavior(score, )
                }
              }
            }

        }
      }
    }
    behavior(Score(Map.empty), RoundState.firstRoundStart, Selection(None))
  }
    
}
