package com.oodic.aoc.y2016

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day19Test extends AnyFlatSpec with Matchers {
  "Day19 - 2016" should "solve first part" in {
    Day19.part1(5) should equal(3)

    Day19.part1(Day19.input) should equal(1808357)
  }

  it should "solve second part" in {
    Day19.part2(5) should equal(2)

    Day19.part2(Day19.input) should equal(1407007)
  }
}
