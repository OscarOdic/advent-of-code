package com.oodic.aoc.y2016

import org.scalatest.{FlatSpec, Matchers}

class Day18Test extends FlatSpec with Matchers {
  "Day18 - 2016" should "answer first part" in {
    Day18.resolveFirst(Day18.input) should be(1989)
  }

  it should "answer second part" in {
    Day18.resolveSecond(Day18.input) should be(19999894)
  }
}
