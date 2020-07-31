package dev.ostrander.discpardy

import ackcord.DiscordClient
import ackcord.data.TextChannel
import ackcord.syntax.TextChannelSyntax
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import dev.ostrander.discpardy.Jeopardy.Clue
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration

object Question {
  sealed trait Command
  case class CreateClue(channel: TextChannel, category: String, clue: Clue) extends Command
  case class ClueCreated(channel: TextChannel, clue: Clue) extends Command
  case class SendAnswer(channel: TextChannel, answer: String) extends Command

  def apply(client: DiscordClient): Behavior[Command] =
    Behaviors.receive[Command] {
      case (ctx, CreateClue(channel, category, clue)) =>
        val message = channel.sendMessage(s"Category: $category\nValue: $$${clue.value}\nQuestion: ${clue.question}")
        ctx.pipeToSelf(
          client.requests.singleFuture(message)
        )(_ => ClueCreated(channel, clue))
        Behaviors.same
      case (ctx, ClueCreated(channel, clue)) =>
        ctx.scheduleOnce(FiniteDuration(10, TimeUnit.SECONDS), ctx.self, SendAnswer(channel, clue.answer))
        Behaviors.same
      case (ctx, SendAnswer(channel, answer)) =>
        client.requests.singleFuture(channel.sendMessage(s"Answer: $answer"))
        Behaviors.same
    }
}
