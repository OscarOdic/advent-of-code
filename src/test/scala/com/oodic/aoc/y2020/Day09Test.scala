package com.oodic.aoc.y2020

import org.scalatest.{FlatSpec, Matchers}

class Day09Test extends FlatSpec with Matchers {
  "Day09 - 2020" should "answer first part" in {
    Day09.resolveFirst(Day09.input) should be(26796446L)
  }

  it should "answer second part" in {
    Day09.resolveSecond(Day09.input) should be(3353494L)
  }
}
