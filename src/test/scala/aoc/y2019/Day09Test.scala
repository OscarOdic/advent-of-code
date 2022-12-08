package aoc.y2019

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day09Test extends AnyFlatSpec with Matchers {
  "Day09 - 2019" should "solve first part" in {
    Day09.part1(Day09.input) should equal(3235019597L)
  }

  it should "solve second part" in {
    Day09.part2(Day09.input) should equal(80274L)
  }
}
