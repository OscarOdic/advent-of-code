package com.oodic.aoc.y2019

import org.scalatest.{FlatSpec, Matchers}

class Day05Test extends FlatSpec with Matchers {
  "Day05 - 2019" should "answer first part" in {
    Day05.resolveFirst(Day05.input) should be(4601506)
  }

  it should "answer second part" in {
    Day05.resolveSecond(Day05.input) should be(5525561)
  }
}
