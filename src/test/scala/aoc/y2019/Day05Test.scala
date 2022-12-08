package aoc.y2019

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day05Test extends AnyFlatSpec with Matchers {
  "Day05 - 2019" should "solve first part" in {
    Day05.part1(Day05.input) should equal(4601506)
  }

  it should "solve second part" in {
    Day05.part2(Day05.input) should equal(5525561)
  }
}
