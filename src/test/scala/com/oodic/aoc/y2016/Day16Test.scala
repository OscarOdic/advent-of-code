package com.oodic.aoc.y2016

import org.scalatest.{FlatSpec, Matchers}

class Day16Test extends FlatSpec with Matchers {
  "Day16 - 2016" should "answer first part" in {
    Day16.resolveFirst(Day16.input) should be("10010101010011101")
  }

  it should "answer second part" in {
    Day16.resolveSecond(Day16.input) should be("01100111101101111")
  }
}
