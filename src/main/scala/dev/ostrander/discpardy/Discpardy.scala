package dev.ostrander.discpardy

import ackcord._
import ackcord.data._
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import ackcord.gateway.GatewayIntents

object Discpardy extends App {
  require(args.nonEmpty, "Please provide a token")
  val token = args.head

  val categories = Jeopardy.loadAllFullCategories

  val clientSettings = ClientSettings(token, intents = GatewayIntents.fromInt(76864))
  import clientSettings.executionContext

  clientSettings.createClient().foreach { client =>
    client.onEventSideEffects { cache => {
      case APIMessage.Ready(_) => println("Now ready")
    }}

    val commands = new Commands(client, categories)

    client.commands.runNewNamedCommand(commands.start)

    client.login()
  }
}
