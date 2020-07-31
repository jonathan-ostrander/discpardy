package dev.ostrander.discpardy

import ackcord.DiscordClient
import ackcord.commands.CommandController
import ackcord.commands.MessageParser
import ackcord.commands.NamedCommand
import akka.actor.typed.ActorRef
import dev.ostrander.discpardy.Jeopardy.Category
import scala.util.Random

class Commands(client: DiscordClient, categories: List[Category], questionActor: ActorRef[Question.Command]) extends CommandController(client.requests) {
  val question: NamedCommand[MessageParser.RemainingAsString] =
    Command.named("!", "question" :: Nil, mustMention = false)
      .parsing[MessageParser.RemainingAsString]
      .withSideEffects { r =>
        val category = categories(Random.nextInt(categories.size))
        val clue = category.clues(Random.nextInt(category.clues.size))
        questionActor ! Question.CreateClue(r.textChannel, category.name, clue)
      }
}
