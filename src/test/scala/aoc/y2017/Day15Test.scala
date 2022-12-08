package aoc.y2017

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day15Test extends AnyFlatSpec with Matchers {
  val testInput = (65L, 8921L)

  "Day15 - 2017" should "solve first part" in {
    Day15.part1(testInput) should equal(588)

    Day15.part1(Day15.input) should equal(650)
  }

  it should "solve second part" in {
    Day15.part2(testInput) should equal(309)

    Day15.part2(Day15.input) should equal(336)
  }
}
