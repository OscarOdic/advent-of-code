package com.oodic.aoc.y2017

import org.scalatest.{FlatSpec, Matchers}

class Day05Test extends FlatSpec with Matchers {
  "Day05 - 2017" should "answer first part" in {
    Day05.resolveFirst(Vector(0,3,0,1,-3)) should be(5)

    Day05.resolveFirst(Day05.input) should be(356945)
  }

  it should "answer second part" in {
    Day05.resolveSecond(Vector(0,3,0,1,-3)) should be(10)

    Day05.resolveSecond(Day05.input) should be(28372145)
  }
}
