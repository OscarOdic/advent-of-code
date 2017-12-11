package com.oodic.aoc

import org.scalatest.{FlatSpec, Matchers}

class Day5Test extends FlatSpec with Matchers {
  it should "answer first part" in {
    Day5.resolveFirst(Vector(0,3,0,1,-3)) should be(5)

    Day5.resolveFirst(Day5.input) should be(356945)
  }

  it should "answer second part" in {
    Day5.resolveSecond(Vector(0,3,0,1,-3)) should be(10)

    Day5.resolveSecond(Day5.input) should be(28372145)
  }
}
