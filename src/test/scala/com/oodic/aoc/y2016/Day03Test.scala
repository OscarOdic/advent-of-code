package com.oodic.aoc.y2016

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day03Test extends AnyFlatSpec with Matchers {
  "Day03 - 2016" should "solve first part" in {
    Day03.part1(Day03.input) should equal(993)
  }

  it should "solve second part" in {
    Day03.part2(Day03.input) should equal(1849)
  }
}
