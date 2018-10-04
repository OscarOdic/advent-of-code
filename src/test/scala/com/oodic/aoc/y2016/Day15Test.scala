package com.oodic.aoc.y2016

import org.scalatest.{FlatSpec, Matchers}

class Day15Test extends FlatSpec with Matchers {
  val testInput: List[String] = List(
    "Disc #1 has 5 positions; at time=0, it is at position 4.",
    "Disc #2 has 2 positions; at time=0, it is at position 1."
  )

  "Day15 - 2016" should "answer first part" in {
    Day15.resolveFirst(testInput) should be(5)

    Day15.resolveFirst(Day15.input) should be(148737)
  }

  it should "answer second part" in {
    Day15.resolveSecond(Day15.input) should be(2353212)
  }
}
