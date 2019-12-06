package com.oodic.aoc.y2019

import org.scalatest.{FlatSpec, Matchers}

class Day02Test extends FlatSpec with Matchers {
  "Day02 - 2019" should "answer first part" in {
    Day02.resolveFirst(Day02.input) should be(4090701)
  }

  it should "answer second part" in {
    Day02.resolveSecond(Day02.input) should be(6421)
  }
}
