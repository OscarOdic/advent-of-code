package aoc.y2023

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day06Test extends AnyFlatSpec with Matchers {
  private val testInput = List(
    "Time:      7  15   30",
    "Distance:  9  40  200"
  )

  "Day06 - 2023" should "solve first part" in {
    Day06.part1(testInput) should equal(288)

    Day06.part1(Day06.input) should equal(1155175)
  }

  it should "solve second part" in {
    Day06.part2(testInput) should equal(71503)

    Day06.part2(Day06.input) should equal(35961505)
  }
}
