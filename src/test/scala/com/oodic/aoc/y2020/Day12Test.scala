package com.oodic.aoc.y2020

import org.scalatest.{FlatSpec, Matchers}

class Day12Test extends FlatSpec with Matchers {
  val testInput = List(
    "F10",
    "N3",
    "F7",
    "R90",
    "F11",
  )


  "Day12 - 2020" should "answer first part" in {
    Day12.resolveFirst(testInput) should be(25)

    Day12.resolveFirst(Day12.input) should be(1010)
  }

  it should "answer second part" in {
    Day12.resolveSecond(testInput) should be(286)

    Day12.resolveSecond(Day12.input) should be(52742)
  }
}
