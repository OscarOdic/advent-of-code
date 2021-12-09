package com.oodic.aoc.y2019

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day04Test extends AnyFlatSpec with Matchers {
  "Day04 - 2019" should "solve first part" in {
    Day04.part1(Day04.input) should equal(960)
  }

  it should "solve second part" in {
    Day04.part2(Day04.input) should equal(626)
  }
}
