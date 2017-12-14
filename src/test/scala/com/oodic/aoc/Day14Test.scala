package com.oodic.aoc

import org.scalatest.{FlatSpec, Matchers}

class Day14Test extends FlatSpec with Matchers {
  val testInput = Day14.getGrid("flqrgnkx")

  it should "answer first part" in {
    Day14.resolveFirst(testInput) should be(8108)

    Day14.resolveFirst(Day14.input) should be(8226)
  }

  it should "answer second part" in {
    Day14.resolveSecond(testInput) should be(1242)

    Day14.resolveSecond(Day14.input) should be(1128)
  }
}
