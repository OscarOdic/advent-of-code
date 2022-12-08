package aoc.y2017

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day19Test extends AnyFlatSpec with Matchers {
  val testInput = Day19.toMap(List(
    "     |         ",
    "     |  +--+   ",
    "     A  |  C   ",
    " F---|----E|--+",
    "     |  |  |  D",
    "     +B-+  +--+"
  ))

  "Day19 - 2017" should "solve first part" in {
    Day19.part1(testInput) should equal("ABCDEF")

    Day19.part1(Day19.input) should equal("SXPZDFJNRL")
  }

  it should "solve second part" in {
    Day19.part2(testInput) should equal(38)

    Day19.part2(Day19.input) should equal(18126)
  }
}