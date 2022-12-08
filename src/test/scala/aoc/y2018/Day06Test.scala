package aoc.y2018

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day06Test extends AnyFlatSpec with Matchers {
  "Day06 - 2018" should "solve first part" in {
    Day06.part1(Day06.input) should equal(4143)
  }

  it should "solve second part" in {
    Day06.part2(Day06.input) should equal(35039)
  }
}
