package com.oodic.aoc.y2020

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day05Test extends AnyFlatSpec with Matchers {
  "Day05 - 2020" should "solve first part" in {
    Day05.part1(Day05.input) should equal(858)
  }

  it should "solve second part" in {
    Day05.part2(Day05.input) should equal(557)
  }
}
