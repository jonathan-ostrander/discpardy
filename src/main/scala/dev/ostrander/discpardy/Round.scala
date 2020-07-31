package dev.ostrander.discpardy

import ackcord.DiscordClient
import ackcord.data.TextChannel
import ackcord.syntax.TextChannelSyntax
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import dev.ostrander.discpardy.Jeopardy

object Round {
  sealed trait Command
  case class PrintRound(channel: TextChannel, round: Jeopardy.Round) extends Command

  def apply(client: DiscordClient): Behavior[Command] = {
    Behaviors.receive[Command] {
      case (ctx, PrintRound(channel, round)) =>
        ctx.log.info(s"Printing round in ${channel.id}")
        ctx.log.info(round.toString)
        client.requests.singleFuture(channel.sendMessage(round.toString))
        Behaviors.same
    }
  }
}
