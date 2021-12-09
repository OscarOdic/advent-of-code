package com.oodic.aoc.y2016

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day18Test extends AnyFlatSpec with Matchers {
  "Day18 - 2016" should "solve first part" in {
    Day18.part1(Day18.input) should equal(1989)
  }

  it should "solve second part" in {
    Day18.part2(Day18.input) should equal(19999894)
  }
}
