package com.oodic.aoc.y2016

import org.scalatest.{FlatSpec, Matchers}

class Day10Test extends FlatSpec with Matchers {
  "Day10 - 2016" should "answer first part" in {
    Day10.resolveFirst(Day10.input) should be(98)
  }

  it should "answer second part" in {
    Day10.resolveSecond(Day10.input) should be(4042)
  }
}
