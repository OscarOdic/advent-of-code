package aoc.y2016

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day13Test extends AnyFlatSpec with Matchers {
  "Day13 - 2016" should "solve first part" in {
    Day13.part1(Day13.input) should equal(86)
  }

  it should "solve second part" in {
    Day13.part2(Day13.input) should equal(127)
  }
}
