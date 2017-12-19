package com.oodic.aoc

import org.scalatest.{FlatSpec, Matchers}

class Day19Test extends FlatSpec with Matchers {
  val testInput = Day19.toMap(List(
    "     |         ",
    "     |  +--+   ",
    "     A  |  C   ",
    " F---|----E|--+",
    "     |  |  |  D",
    "     +B-+  +--+"
  ))

  it should "answer first part" in {
    Day19.resolveFirst(testInput) should be("ABCDEF")

    Day19.resolveFirst(Day19.input) should be("SXPZDFJNRL")
  }

  it should "answer second part" in {
    Day19.resolveSecond(testInput) should be(38)

    Day19.resolveSecond(Day19.input) should be(18126)
  }
}