package com.oodic.aoc.y2017

import org.scalatest.{FlatSpec, Matchers}

class Day03Test extends FlatSpec with Matchers {
  "Day03 - 2017" should "answer first part" in {
    Day03.resolveFirst(12) should be(3)
    Day03.resolveFirst(23) should be(2)
    Day03.resolveFirst(1024) should be(31)

    Day03.resolveFirst(Day03.input) should be(552)
  }

  it should "answer second part" in {
    Day03.resolveSecond(Day03.input) should be(330785)
  }
}
