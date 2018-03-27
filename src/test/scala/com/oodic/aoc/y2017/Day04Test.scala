package com.oodic.aoc.y2017

import org.scalatest.{FlatSpec, Matchers}

class Day04Test extends FlatSpec with Matchers {
  "Day04 - 2017" should "answer first part" in {
    Day04.resolveFirst(Day04.input) should be(477)
  }

  it should "answer second part" in {
    Day04.resolveSecond(Day04.input) should be(167)
  }
}
