package aoc.y2017

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day03Test extends AnyFlatSpec with Matchers {
  "Day03 - 2017" should "solve first part" in {
    Day03.part1(12) should equal(3)
    Day03.part1(23) should equal(2)
    Day03.part1(1024) should equal(31)

    Day03.part1(Day03.input) should equal(552)
  }

  it should "solve second part" in {
    Day03.part2(Day03.input) should equal(330785)
  }
}
