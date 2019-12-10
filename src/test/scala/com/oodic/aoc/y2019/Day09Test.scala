package com.oodic.aoc.y2019

import org.scalatest.{FlatSpec, Matchers}

class Day09Test extends FlatSpec with Matchers {
  "Day09 - 2019" should "answer first part" in {
    Day09.resolveFirst(Day09.input) should be(3235019597L)
  }

  it should "answer second part" in {
    Day09.resolveSecond(Day09.input) should be(80274L)
  }
}
