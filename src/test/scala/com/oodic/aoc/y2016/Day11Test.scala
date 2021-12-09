package com.oodic.aoc.y2016

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day11Test extends AnyFlatSpec with Matchers {
  "Day11 - 2016" should "solve first part" in {
    Day11.part1(Day11.input) should equal(31)
  }

  it should "solve second part" in {
    Day11.part2(Day11.input) should equal(55)
  }
}