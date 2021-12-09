package com.oodic.aoc.y2016

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day14Test extends AnyFlatSpec with Matchers {
  "Day14 - 2016" should "solve first part" in {
    Day14.part1("abc") should equal(22728)

    Day14.part1(Day14.input) should equal(18626)
  }

  it should "solve second part" in {
    Day14.part2(Day14.input) should equal(20092)
  }
}
