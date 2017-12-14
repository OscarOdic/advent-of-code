package com.oodic.aoc

import org.scalatest.{FlatSpec, Matchers}

class Day08Test extends FlatSpec with Matchers {
  val testInput = List(
    "b inc 5 if a > 1",
    "a inc 1 if b < 5",
    "c dec -10 if a >= 1",
    "c inc -20 if c == 10"
  )

  it should "answer first part" in {
    Day08.resolveFirst(testInput) should be(1)

    Day08.resolveFirst(Day08.input) should be(5075)
  }

  it should "answer second part" in {
    Day08.resolveSecond(testInput) should be(10)

    Day08.resolveSecond(Day08.input) should be(7310)
  }
}
