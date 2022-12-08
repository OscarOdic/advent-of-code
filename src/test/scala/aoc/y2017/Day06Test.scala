package aoc.y2017

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day06Test extends AnyFlatSpec with Matchers {
  "Day06 - 2017" should "solve first part" in {
    Day06.part1(Vector(0,2,7,0)) should equal(5)

    Day06.part1(Day06.input) should equal(3156)
  }

  it should "solve second part" in {
    Day06.part2(Vector(0,2,7,0)) should equal(4)

    Day06.part2(Day06.input) should equal(1610)
  }
}
