package com.oodic.aoc.y2016

import org.scalatest.{FlatSpec, Matchers}

class Day11Test extends FlatSpec with Matchers {
  "Day11 - 2016" should "answer first part" in {
    Day11.resolveFirst(Day11.input) should be(31)
  }

  it should "answer second part" in {
    Day11.resolveSecond(Day11.input) should be(55)
  }
}