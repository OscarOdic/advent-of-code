package com.oodic.aoc.y2016

import org.scalatest.{FlatSpec, Matchers}

class Day13Test extends FlatSpec with Matchers {
  "Day13 - 2016" should "answer first part" in {
    Day13.resolveFirst(Day13.input) should be(86)
  }

  it should "answer second part" in {
    Day13.resolveSecond(Day13.input) should be(127)
  }
}
