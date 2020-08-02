package dev.ostrander.discpardy

import scala.io.Source
import scala.util.Random
import spray.json.DefaultJsonProtocol._
import spray.json.JsonFormat
import spray.json.enrichString

package object model {
  private[model] object Parsed {
    case class RawClue(value: String, question: String, answer: String) {
      def toClue: Clue = Clue(value.drop(1).toInt, question, answer)
    }

    case class RawCategory(
      category: String,
      clues: List[Option[RawClue]],
    ) {
      def toCategory: Option[Category] = {
        val flattened = clues.flatten
        if (flattened.size == clues.size && clues.size == 5) Some(Category(category, flattened.map(_.toClue)))
        else None
      }
    }

    case class RawFinalJeopardy(category: String, question: String, answer: String) {
      def toFinalJeopardy: FinalJeopardy = FinalJeopardy(category, question, answer)
    }

    case class RawGame(first_round: List[RawCategory], second_round: List[RawCategory], `final`: RawFinalJeopardy) {
      def firstRounds: List[Category] = first_round.flatMap(_.toCategory)
      def secondReounds: List[Category] = second_round.flatMap(_.toCategory)
    }

    implicit val clueFormat: JsonFormat[RawClue] = jsonFormat3(RawClue)
    implicit val categoryFormat: JsonFormat[RawCategory] = jsonFormat2(RawCategory)
    implicit val finalJeopardyFormat: JsonFormat[RawFinalJeopardy] = jsonFormat3(RawFinalJeopardy)
    implicit val gameFormat: JsonFormat[RawGame] = jsonFormat3(RawGame)
  }

  case class Clue(value: Int, question: String, answer: String) {
    def dollars: String = s"$$$value"
  }

  case class Category(
    name: String,
    clues: List[Clue],
  ) {
    require(clues.size == 5, "Categories must have exactly 5 clues")
  }

  case class FinalJeopardy(category: String, question: String, answer: String)

  case class Round(categories: List[Category]) {
    private[this] def padding(v: String, to: Int): String =
      if (v.size >= to) v
      else {
        val p = {
          val diff = to - v.size
          if (diff % 2 == 0) (diff / 2, diff / 2)
          else if (v.size % 2 == 0) (diff / 2 + 1, diff / 2)
          else (diff / 2, diff / 2 + 1)
        }
        (0 until p._1).map(_ => " ").mkString + v + (0 until p._2).map(_ => " ").mkString
      }
    override def toString: String = {
      val header = categories.map(c => padding(c.name, 10)).mkString("| ", " | ", " |")
      val separator = "\n" + (0 until header.length).map(_ => "-").mkString + "\n"
      val sizes = categories.map(_.name.size.max(10))
      (
        header :: categories.map(_.clues).transpose.map(
          _.zip(sizes).map { case (v, s) => padding("$" + v.value.toString, s) }.mkString("| ", " | ", " |"),
        )
      ).mkString("```" + separator, separator, separator + "```")
    }
  }

  case class Game(firstRound: Round, secondRound: Round, finalJeopardy: FinalJeopardy)

  case class LoadedCategories(
    firstRounds: List[Category],
    secondRounds: List[Category],
    finalJeopardies: List[FinalJeopardy],
  ) {
    def newGame: Game = Game(
      Round(Random.shuffle(firstRounds).take(6)),
      Round(Random.shuffle(secondRounds).take(6)),
      Random.shuffle(finalJeopardies).head,
    )
  }
  object LoadedCategories {
    def load: LoadedCategories = {
      val games = Source
        .fromResource("jeopardy.json")
        .getLines()
        .mkString("\n")
        .parseJson
        .convertTo[List[Parsed.RawGame]]
      LoadedCategories(
        games.flatMap(_.firstRounds),
        games.flatMap(_.secondReounds),
        games.map(_.`final`.toFinalJeopardy),
      )
    }
  }
}
