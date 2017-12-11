package com.oodic.aoc

import org.scalatest.{FlatSpec, Matchers}

class Day8Test extends FlatSpec with Matchers {
  val testInput = List(
    "b inc 5 if a > 1",
    "a inc 1 if b < 5",
    "c dec -10 if a >= 1",
    "c inc -20 if c == 10"
  )

  it should "answer first part" in {
    Day8.resolveFirst(testInput) should be(1)

    Day8.resolveFirst(Day8.input) should be(5075)
  }

  it should "answer second part" in {
    Day8.resolveSecond(testInput) should be(10)

    Day8.resolveSecond(Day8.input) should be(7310)
  }
}
