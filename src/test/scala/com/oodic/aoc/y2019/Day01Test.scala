package com.oodic.aoc.y2019

import org.scalatest.{FlatSpec, Matchers}

class Day01Test extends FlatSpec with Matchers {
  "Day01 - 2019" should "answer first part" in {
    Day01.resolveFirst(List(12)) should be(2)
    Day01.resolveFirst(List(14)) should be(2)
    Day01.resolveFirst(List(1969)) should be(654)
    Day01.resolveFirst(List(100756)) should be(33583)
    Day01.resolveFirst(List(12, 14, 1969, 100756)) should be(34241)

    Day01.resolveFirst(Day01.input) should be(3302760)
  }

  it should "answer second part" in {
    Day01.resolveSecond(List(12)) should be(2)
    Day01.resolveSecond(List(14)) should be(2)
    Day01.resolveSecond(List(1969)) should be(966)
    Day01.resolveSecond(List(100756)) should be(50346)
    Day01.resolveSecond(List(12, 14, 1969, 100756)) should be(51316)

    Day01.resolveSecond(Day01.input) should be(4951265)
  }
}
