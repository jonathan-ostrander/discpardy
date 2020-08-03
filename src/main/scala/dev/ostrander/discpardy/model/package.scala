package dev.ostrander.discpardy

import org.apache.commons.text.similarity.JaccardSimilarity
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

  private[model] val jaccard = new JaccardSimilarity()
  private[model] def similarity(left: String, right: String): Double = jaccard(left, right)

  trait Matchable {
    def sanitizable: String
    def sanitized: String = sanitizable.filter(_.isLetter).toLowerCase
    def matchScore(message: String): Double = similarity(sanitized, message.filter(_.isLetter).toLowerCase)
  }

  case class Clue(value: Int, question: String, answer: String) extends Matchable {
    def dollars: String = s"$$$value"
    def sanitizable: String = answer
  }

  case class Category(
    name: String,
    clues: List[Clue],
  ) extends Matchable {
    require(clues.size == 5, "Categories must have exactly 5 clues")

    def sanitizable: String = name
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

    def clue(index: ClueIndex): Clue = categories(index.category).clues(index.clue)
    def getClueIndex(message: String): ClueIndex = {
      val category = categories.zipWithIndex.maxBy { case (c, _) => c.matchScore(message) }._2
      val clueIndex = categories(category).clues.map(_.value.toString).zipWithIndex.maxBy {
        case (d, _) => similarity(d, message.filter(_.isDigit))
      }._2
      ClueIndex(category, clueIndex)
    }
  }

  case class Game(firstRound: Round, secondRound: Round, finalJeopardy: FinalJeopardy)

  case class ClueIndex(category: Int, clue: Int) {
    require(category >= 0 && category < 6, "there can only be 6 categories in a round")
    require(clue >= 0 && clue < 5, "there can only be 5 clues in a category")
  }
  object ClueIndex {
    def random: ClueIndex = ClueIndex(Random.nextInt(6), Random.nextInt(5))
  }

  sealed trait RoundNumber
  object RoundNumber {
    case object Jeopardy extends RoundNumber
    case object DoubleJeopardy extends RoundNumber
  }

  sealed trait GameProgress
  object GameProgress {
    case class RoundState(
      roundNumber: RoundNumber,
      dailyDoubles: Set[ClueIndex],
      used: Set[ClueIndex],
    ) extends GameProgress {
      def isComplete: Boolean = used.size == 30
      def next(clue: ClueIndex): RoundState = this.copy(used = used + clue)
    }
    
    def firstRoundStart: RoundState = RoundState(RoundNumber.Jeopardy, Set(ClueIndex.random), Set.empty)
    def secondRoundStart: RoundState = RoundState(RoundNumber.DoubleJeopardy, Set(ClueIndex.random, ClueIndex.random), Set.empty)

    case object FinalJeopardy extends GameProgress
  }

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
