package dev.ostrander.discpardy

import ackcord.DiscordClient
import ackcord.commands.CommandController
import ackcord.commands.MessageParser
import ackcord.commands.NamedCommand
import akka.actor.typed.ActorRef
import dev.ostrander.discpardy.Jeopardy.Category
import dev.ostrander.discpardy.Jeopardy.FinalJeopardy
import dev.ostrander.discpardy.Jeopardy.FullGame
import scala.util.Random

class Commands(
  client: DiscordClient,
  categories: List[Category],
  finals: List[FinalJeopardy],
  questionActor: ActorRef[Question.Command],
  roundActor: ActorRef[Round.Command],
) extends CommandController(client.requests) {
  val question: NamedCommand[MessageParser.RemainingAsString] =
    Command.named("!", "question" :: Nil, mustMention = false)
      .parsing[MessageParser.RemainingAsString]
      .withSideEffects { r =>
        val category = categories(Random.nextInt(categories.size))
        val clue = category.clues(Random.nextInt(category.clues.size))
        questionActor ! Question.CreateClue(r.textChannel, category.name, clue)
      }
  val round: NamedCommand[MessageParser.RemainingAsString] =
    Command.named("!", "round" :: Nil, mustMention = false)
      .parsing[MessageParser.RemainingAsString]
      .withSideEffects { r =>
        val game = FullGame.fromCategories(categories, finals)
        roundActor ! Round.PrintRound(r.textChannel, game.firstRound)
      }
}
