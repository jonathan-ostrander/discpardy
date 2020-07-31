package dev.ostrander.discpardy

import spray.json._
import spray.json.DefaultJsonProtocol._
import scala.io.Source
import scala.util.Random

object Jeopardy {
  case class Clue(value: Int, question: String, answer: String)
  case class RawClue(value: String, question: String, answer: String) {
    def toClue: Clue = Clue(value.drop(1).toInt, question, answer)
  }

  case class Category(
    name: String,
    clues: List[Clue],
  )

  case class IncompleteCategory(
    category: String,
    clues: List[Option[RawClue]],
  ) {
    def toCategory: Option[Category] = {
      val flattened = clues.flatten
      if (flattened.size == clues.size && clues.size == 5) Some(Category(category, flattened.map(_.toClue)))
      else None
    }
  }

  case class FinalJeopardy(category: String, question: String, answer: String)

  case class Game(first_round: List[IncompleteCategory], second_round: List[IncompleteCategory], `final`: FinalJeopardy) {
    def categories: List[Category] = first_round.flatMap(_.toCategory) ++ second_round.flatMap(_.toCategory)
  }

  implicit val clueFormat: JsonFormat[RawClue] = jsonFormat3(RawClue)
  implicit val categoryFormat: JsonFormat[IncompleteCategory] = jsonFormat2(IncompleteCategory)
  implicit val finalJeopardyFormat: JsonFormat[FinalJeopardy] = jsonFormat3(FinalJeopardy)
  implicit val gameFormat: JsonFormat[Game] = jsonFormat3(Game)

  def loadAll: (List[Category], List[FinalJeopardy]) = {
    val games =
      Source
        .fromResource("jeopardy.json")
        .getLines()
        .mkString("\n")
        .parseJson
        .convertTo[List[Game]]
    (games.flatMap(_.categories), games.map(_.`final`))
  }

  case class Round(categories: List[Category]) {
    private[this] def padding(v: String, to: Int): String =
      if (v.size >= to) v else {
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
          _.zip(sizes).map { case (v, s) => padding("$" + v.value.toString, s) }.mkString("| ", " | ", " |")
        )
      ).mkString("```" + separator, separator, separator + "```")
    }
  }
  case class FullGame(firstRound: Round, secondRound: Round, finalJeopardy: FinalJeopardy)
  object FullGame {
    def fromCategories(categories: List[Category], finalJeopardies: List[FinalJeopardy]): FullGame = {
      val (firstRounds, secondRounds) = categories.partition(_.clues.head.value == 100)
      FullGame(
        Round(Random.shuffle(firstRounds).take(6)),
        Round(Random.shuffle(secondRounds).take(6)),
        Random.shuffle(finalJeopardies).head,
      )
    }
  }
}
