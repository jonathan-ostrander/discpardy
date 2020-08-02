package dev.ostrander.discpardy

import ackcord.commands.CommandController
import ackcord.commands.MessageParser
import ackcord.commands.NamedCommand
import ackcord.data.TextChannel
import ackcord.requests.Requests
import akka.actor.typed.ActorRef
import dev.ostrander.discpardy.actor.GameManager
import dev.ostrander.discpardy.actor.Question

class Commands(
  requests: Requests,
  questionActor: ActorRef[Question.Command],
  gameActor: ActorRef[GameManager.Command],
) extends CommandController(requests) {
  private[this] def create[C](
    command: String,
    actor: ActorRef[C],
    c: TextChannel => C,
  ): NamedCommand[MessageParser.RemainingAsString] =
    Command.named("!", command :: Nil, mustMention = false)
      .parsing[MessageParser.RemainingAsString]
      .withSideEffects { r =>
        actor ! c(r.textChannel)
      }

  val commands: List[NamedCommand[MessageParser.RemainingAsString]] = List(
    create("question", questionActor, Question.CreateClue.apply),
    create("game", gameActor, GameManager.CreateGame.apply),
  )
}
