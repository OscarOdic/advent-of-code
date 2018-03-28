package com.oodic.aoc.y2016

import org.scalatest.{FlatSpec, Matchers}

class Day03Test extends FlatSpec with Matchers {
  "Day03 - 2016" should "answer first part" in {
    Day03.resolveFirst(Day03.input) should be(993)
  }

  it should "answer second part" in {
    Day03.resolveSecond(Day03.input) should be(1849)
  }
}
