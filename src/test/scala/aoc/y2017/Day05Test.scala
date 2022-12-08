package aoc.y2017

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day05Test extends AnyFlatSpec with Matchers {
  "Day05 - 2017" should "solve first part" in {
    Day05.part1(Vector(0,3,0,1,-3)) should equal(5)

    Day05.part1(Day05.input) should equal(356945)
  }

  it should "solve second part" in {
    Day05.part2(Vector(0,3,0,1,-3)) should equal(10)

    Day05.part2(Day05.input) should equal(28372145)
  }
}
