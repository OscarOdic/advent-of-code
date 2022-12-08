package aoc.y2020

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day17Test extends AnyFlatSpec with Matchers {
  val testInput = List(
    ".#.",
    "..#",
    "###"
  )

  "Day17 - 2020" should "solve first part" in {
    Day17.part1(testInput) should equal(112)

    Day17.part1(Day17.input) should equal(230)
  }

  it should "solve second part" in {
    Day17.part2(testInput) should equal(848)

    Day17.part2(Day17.input) should equal(1600)
  }
}
