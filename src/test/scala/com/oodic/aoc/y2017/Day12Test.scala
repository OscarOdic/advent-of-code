package com.oodic.aoc.y2017

import org.scalatest.{FlatSpec, Matchers}

class Day12Test extends FlatSpec with Matchers {
  val testInput = List(
    "0 <-> 2",
    "1 <-> 1",
    "2 <-> 0, 3, 4",
    "3 <-> 2, 4",
    "4 <-> 2, 3, 6",
    "5 <-> 6",
    "6 <-> 4, 5"
  )

  "Day12 - 2017" should "answer first part" in {
    Day12.resolveFirst(testInput) should be(6)

    Day12.resolveFirst(Day12.input) should be(306)
  }

  it should "answer second part" in {
    Day12.resolveSecond(testInput) should be(2)

    Day12.resolveSecond(Day12.input) should be(200)
  }
}
