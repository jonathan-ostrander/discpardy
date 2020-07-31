package dev.ostrander.discpardy

import ackcord.APIMessage.MessageMessage
import ackcord.DiscordClient
import ackcord.data.TextChannel
import ackcord.data.TextChannelId
import ackcord.syntax.MessageSyntax
import ackcord.syntax.TextChannelSyntax
import akka.actor.typed.ActorRef
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import dev.ostrander.discpardy.Jeopardy.Clue
import java.util.concurrent.TimeUnit
import org.apache.commons.text.similarity.JaccardSimilarity
import scala.collection.concurrent.TrieMap
import scala.concurrent.duration.FiniteDuration

object Question {
  sealed trait Command
  case class CreateClue(channel: TextChannel, category: String, clue: Clue) extends Command
  case class ClueCreated(channel: TextChannel, clue: Clue) extends Command
  case class ClearAnswer(textChannelId: TextChannelId) extends Command

  def apply(client: DiscordClient): Behavior[Command] = {
    val channelMap: TrieMap[TextChannelId, ActorRef[Answer.Command]] = TrieMap.empty[TextChannelId, ActorRef[Answer.Command]]

    client.onEventSideEffects(_ => {
      case mm: MessageMessage =>
        channelMap.get(mm.message.channelId).foreach { ref =>
          ref ! Answer.Message(mm)
        }
    })

    Behaviors.receive[Command] {
      case (ctx, CreateClue(channel, category, clue)) =>
        ctx.log.info(s"Creating clue in ${channel.id}")
        val message = channel.sendMessage(s"Category: $category\nValue: $$${clue.value}\nQuestion: ${clue.question}")
        ctx.pipeToSelf(
          client.requests.singleFuture(message)
        )(_ => ClueCreated(channel, clue))
        Behaviors.same
      case (ctx, ClueCreated(channel, clue)) =>
        ctx.log.info(s"Clue created in ${channel.id}")
        val answer = ctx.spawn(Answer(client, channel, clue, ctx.self), channel.id.asString)
        channelMap.putIfAbsent(channel.id, answer) match {
          case None =>
            ctx.log.info(s"No existing actor found: scheduling timeout")
            ctx.scheduleOnce(FiniteDuration(10, TimeUnit.SECONDS), answer, Answer.Timeout)
          case Some(_) =>
            ctx.log.warn(s"Existing question already found for ${channel.id}, stopping spawned actor")
            answer ! Answer.Stop
        }
        Behaviors.same
      case (ctx, ClearAnswer(channelId)) =>
        ctx.log.info(s"Clearing ${channelId} from actor map.")
        channelMap.remove(channelId)
        Behaviors.same
    }
  }
}

object Answer {
  sealed trait Command
  case class Message(value: MessageMessage) extends Command
  case object Timeout extends Command
  case object Stop extends Command

  val threshold = 0.6
  val similarity = new JaccardSimilarity()
  def correct(left: String, right: String): Boolean = similarity(left, right) >= threshold

  def apply(client: DiscordClient, channel: TextChannel, clue: Clue, parent: ActorRef[Question.Command]): Behavior[Command] = {
    Behaviors.receive[Command] {
      case (ctx, Message(value)) =>
        if (value.message.authorUser(value.cache.current).exists(_.bot.exists(identity))) {
          Behaviors.same
        } else {
          val messageAnswer = value.message.content.toLowerCase()
          val score = similarity(messageAnswer, clue.answer.toLowerCase())
          ctx.log.info(s"${messageAnswer} and ${clue.answer} are ${score} similar.")
          if (correct(messageAnswer, clue.answer)) {
            client.requests.singleFuture(value.message.createReaction("✅"))
            client.requests.singleFuture(channel.sendMessage(s""""$messageAnswer" is correct! (Actual answer: ${clue.answer}, similarity score: ${score})"""))
            parent ! Question.ClearAnswer(channel.id)
            Behaviors.stopped
          } else {
            client.requests.singleFuture(value.message.createReaction("❌"))
            Behaviors.same
          }
        }
      case (ctx, Timeout) =>
        ctx.log.info("No correct answer sent. Alerting.")
        client.requests.singleFuture(channel.sendMessage(s"Answer: ${clue.answer}"))
        parent ! Question.ClearAnswer(channel.id)
        Behaviors.stopped
      case (ctx, Stop) =>
        ctx.log.info("Stopping self.")
        parent ! Question.ClearAnswer(channel.id)
        Behaviors.stopped
    }
  }
}
