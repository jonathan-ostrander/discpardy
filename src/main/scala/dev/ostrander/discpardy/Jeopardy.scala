package dev.ostrander.discpardy

import spray.json._
import spray.json.DefaultJsonProtocol._
import scala.io.Source
import shapeless.Fin

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
      if (flattened.size == clues.size) Some(Category(category, flattened.map(_.toClue)))
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

  def loadAllFullCategories: List[Category] = {
    val games =
      Source
        .fromResource("jeopardy.json")
        .getLines()
        .mkString("\n")
        .parseJson
        .convertTo[List[Game]]
    games.flatMap(_.categories)
  }
}
