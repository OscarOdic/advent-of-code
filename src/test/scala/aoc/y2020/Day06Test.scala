package aoc.y2020

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day06Test extends AnyFlatSpec with Matchers {
  val testInput = List(
    List("abc"),

    List("a",
      "b",
      "c"),

    List("ab",
      "ac"),

    List("a",
      "a",
      "a",
      "a"),

    List("b")
  )

  "Day06 - 2020" should "solve first part" in {
    Day06.part1(testInput) should equal(11)

    Day06.part1(Day06.input) should equal(6565)
  }

  it should "solve second part" in {
    Day06.part2(testInput) should equal(6)

    Day06.part2(Day06.input) should equal(3137)
  }
}
