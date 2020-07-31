package dev.ostrander.discpardy

import ackcord.APIMessage
import ackcord.ClientSettings
import ackcord.gateway.GatewayIntents
import akka.actor.typed.ActorSystem

object Discpardy extends App {
  require(args.nonEmpty, "Please provide a token")
  val token = args.head

  val (categories, finals) = Jeopardy.loadAll

  val clientSettings = ClientSettings(token, intents = GatewayIntents.fromInt(76864))
  import clientSettings.executionContext

  clientSettings.createClient().foreach { client =>
    client.onEventSideEffects { cache => {
      case APIMessage.Ready(_) => clientSettings.system.log.info("Now ready")
    }}

    val question = ActorSystem(Question(client), "Questions")
    val round = ActorSystem(Round(client), "Round")
    val commands = new Commands(client, categories, finals, question, round)

    client.commands.runNewNamedCommand(commands.question)
    client.commands.runNewNamedCommand(commands.round)

    client.login()
  }
}
