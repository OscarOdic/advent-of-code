package com.oodic.aoc

import org.scalatest.{FlatSpec, Matchers}

class Day4Test extends FlatSpec with Matchers {
  it should "answer first part" in {
    Day4.resolveFirst(Day4.input) should be(477)
  }

  it should "answer second part" in {
    Day4.resolveSecond(Day4.input) should be(167)
  }
}
