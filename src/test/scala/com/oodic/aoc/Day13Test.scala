package com.oodic.aoc

import org.scalatest.{FlatSpec, Matchers}

class Day13Test extends FlatSpec with Matchers {
  val testInput = List(
    "0: 3",
    "1: 2",
    "4: 4",
    "6: 4"
  )

  it should "answer first part" in {
    Day13.resolveFirst(testInput) should be(24)

    Day13.resolveFirst(Day13.input) should be(632)
  }

  it should "answer second part" in {
    Day13.resolveSecond(testInput) should be(10)

    Day13.resolveSecond(Day13.input) should be(3849742)
  }
}
