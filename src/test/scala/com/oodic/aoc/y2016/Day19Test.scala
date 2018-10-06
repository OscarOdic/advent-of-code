package com.oodic.aoc.y2016

import org.scalatest.{FlatSpec, Matchers}

class Day19Test extends FlatSpec with Matchers {
  "Day19 - 2016" should "answer first part" in {
    Day19.resolveFirst(5) should be(3)

    Day19.resolveFirst(Day19.input) should be(1808357)
  }

  it should "answer second part" in {
    Day19.resolveSecond(5) should be(2)

    Day19.resolveSecond(Day19.input) should be(1407007)
  }
}
