package aoc.y2020

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day18Test extends AnyFlatSpec with Matchers {
  val testInput = List(
    "2 * 3 + (4 * 5)",
    "5 + (8 * 3 + 9 + 3 * 4 * 3)",
    "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))",
    "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"
  )

  "Day18 - 2020" should "solve first part" in {
    Day18.part1(testInput) should equal(26335)

    Day18.part1(Day18.input) should equal(67800526776934L)
  }

  it should "solve second part" in {

    Day18.part2(testInput) should equal(693891)

    Day18.part2(Day18.input) should equal(340789638435483L)
  }
}
