package com.oodic.aoc

import org.scalatest.{FlatSpec, Matchers}

class Day06Test extends FlatSpec with Matchers {
  it should "answer first part" in {
    Day06.resolveFirst(Vector(0,2,7,0)) should be(5)

    Day06.resolveFirst(Day06.input) should be(3156)
  }

  it should "answer second part" in {
    Day06.resolveSecond(Vector(0,2,7,0)) should be(4)

    Day06.resolveSecond(Day06.input) should be(1610)
  }
}
