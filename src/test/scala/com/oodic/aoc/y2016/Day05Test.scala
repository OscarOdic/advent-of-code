package com.oodic.aoc.y2016

import org.scalatest.{FlatSpec, Matchers}

class Day05Test extends FlatSpec with Matchers {
  "Day05 - 2016" should "answer first part" in {
    Day05.resolveFirst("abc") should be("18f47a30")

    Day05.resolveFirst(Day05.input) should be("1a3099aa")
  }

  it should "answer second part" in {
    Day05.resolveSecond("abc") should be("05ace8e3")

    Day05.resolveSecond(Day05.input) should be("694190cd")
  }
}
