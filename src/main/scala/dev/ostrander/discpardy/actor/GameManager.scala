package dev.ostrander.discpardy.actor

import ackcord.APIMessage.MessageMessage
import ackcord.DiscordClient
import ackcord.data.TextChannel
import ackcord.data.TextChannelId
import ackcord.data.TextGuildChannel
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import dev.ostrander.discpardy.model.LoadedCategories
import scala.collection.concurrent.TrieMap

object GameManager {
  sealed trait Command
  case class CreateGame(textChannel: TextChannel) extends Command

  def apply(client: DiscordClient, jeopardy: LoadedCategories): Behavior[Command] = {
    val channelMap: TrieMap[TextChannelId, Any] = TrieMap.empty[TextChannelId, Any]

    client.onEventSideEffects(_ => {
      case mm: MessageMessage =>
        channelMap.get(mm.message.channelId).foreach { ref =>
          // ref ! Answer.Message(mm)
        }
    })

    Behaviors.receive[Command] {
      case (ctx, CreateGame(channel)) =>
        val id = channel match {
          case c: TextGuildChannel => c.name
          case _ => channel.id.toString
        }
        ctx.log.info(s"Creating game in $id")
        // val ref = ctx.spawn(...)

        Behaviors.same
    }
  }
}
