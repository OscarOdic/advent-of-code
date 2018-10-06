package com.oodic.aoc.y2016

import org.scalatest.{FlatSpec, Matchers}

class Day20Test extends FlatSpec with Matchers {
  "Day20 - 2016" should "answer first part" in {
    Day20.resolveFirst(Day20.input) should be(14975795L)
  }

  it should "answer second part" in {
    Day20.resolveSecond(Day20.input) should be(101)
  }
}
