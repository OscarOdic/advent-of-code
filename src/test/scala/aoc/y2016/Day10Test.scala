package aoc.y2016

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day10Test extends AnyFlatSpec with Matchers {
  "Day10 - 2016" should "solve first part" in {
    Day10.part1(Day10.input) should equal(98)
  }

  it should "solve second part" in {
    Day10.part2(Day10.input) should equal(4042)
  }
}
