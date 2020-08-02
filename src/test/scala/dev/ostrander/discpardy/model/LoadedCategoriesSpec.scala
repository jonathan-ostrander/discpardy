package dev.ostrander.discpardy.model

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class LoadedCategoriesSpec extends AnyFlatSpec with should.Matchers {
  "LoadedCategories.load" should "load jeopardy.json resource" in {
    val jeopardy = LoadedCategories.load
    jeopardy.firstRounds.nonEmpty should be (true)
    jeopardy.secondRounds.nonEmpty should be (true)
    jeopardy.finalJeopardies.nonEmpty should be (true)
  }
}
