package dev.ostrander.discpardy

import ackcord.APIMessage
import ackcord.ClientSettings
import ackcord.gateway.GatewayIntents
import akka.actor.typed.ActorSystem
import dev.ostrander.discpardy.actor.GameManager
import dev.ostrander.discpardy.actor.Question
import dev.ostrander.discpardy.model.LoadedCategories

object Discpardy extends App {
  require(args.nonEmpty, "Please provide a token")
  val token = args.head

  val jeopardy = LoadedCategories.load

  val clientSettings = ClientSettings(token, intents = GatewayIntents.fromInt(76864))
  import clientSettings.executionContext

  clientSettings.createClient().foreach { client =>
    client.onEventSideEffects { cache =>
      {
        case APIMessage.Ready(_) => clientSettings.system.log.info("Now ready")
      }
    }

    val question = ActorSystem(Question(client, jeopardy.firstRounds ++ jeopardy.secondRounds, None), "Questions")
    val game = ActorSystem(GameManager(client, jeopardy), "Games")
    val commands = new Commands(client.requests, question, game)

    client.commands.bulkRunNamed(commands.commands: _*)

    client.login()
  }
}
