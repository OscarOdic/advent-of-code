package com.oodic.aoc.y2016

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day05Test extends AnyFlatSpec with Matchers {
  "Day05 - 2016" should "solve first part" in {
    Day05.part1("abc") should equal("18f47a30")

    Day05.part1(Day05.input) should equal("1a3099aa")
  }

  it should "solve second part" in {
    Day05.part2("abc") should equal("05ace8e3")

    Day05.part2(Day05.input) should equal("694190cd")
  }
}
