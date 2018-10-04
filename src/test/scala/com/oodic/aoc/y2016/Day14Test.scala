package com.oodic.aoc.y2016

import org.scalatest.{FlatSpec, Matchers}

class Day14Test extends FlatSpec with Matchers {
  "Day14 - 2016" should "answer first part" in {
    Day14.resolveFirst("abc") should be(22728)

    Day14.resolveFirst(Day14.input) should be(18626)
  }

  it should "answer second part" in {
    Day14.resolveSecond(Day14.input) should be(20092)
  }
}
