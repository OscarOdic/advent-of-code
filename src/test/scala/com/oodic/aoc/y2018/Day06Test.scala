package com.oodic.aoc.y2018

import org.scalatest.{FlatSpec, Matchers}

class Day06Test extends FlatSpec with Matchers {
  "Day06 - 2018" should "answer first part" in {
    Day06.resolveFirst(Day06.input) should be(4143)
  }

  it should "answer second part" in {
    Day06.resolveSecond(Day06.input) should be(35039)
  }
}
