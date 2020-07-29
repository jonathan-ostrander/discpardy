package dev.ostrander.discpardy

import ackcord.DiscordClient
import ackcord.commands.CommandController
import ackcord.commands.MessageParser
import ackcord.commands.NamedCommand
import ackcord.requests.Requests
import ackcord.syntax.TextChannelSyntax
import akka.actor.Actor
import akka.actor.Timers
import dev.ostrander.discpardy.Jeopardy.Category
import scala.util.Random

class Commands(client: DiscordClient, categories: List[Category]) extends CommandController(client.requests) {
  val start: NamedCommand[MessageParser.RemainingAsString] =
    Command.named("!", "jeopardy" :: Nil, mustMention = false)
      .parsing[MessageParser.RemainingAsString]
      .async { implicit r =>
        val category = categories(Random.nextInt(categories.size))
        val clue = category.clues(Random.nextInt(category.clues.size))
        for {
          _ <- client.requests.singleFuture(r.textChannel.sendMessage(s"Category: ${category.name}\nQuestion: ${clue.question}\nAnswer: ||${clue.answer}||"))
        } yield ()
      }
}
