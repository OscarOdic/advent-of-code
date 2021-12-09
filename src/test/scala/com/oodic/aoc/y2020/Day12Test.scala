package com.oodic.aoc.y2020

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day12Test extends AnyFlatSpec with Matchers {
  val testInput = List(
    "F10",
    "N3",
    "F7",
    "R90",
    "F11",
  )


  "Day12 - 2020" should "solve first part" in {
    Day12.part1(testInput) should equal(25)

    Day12.part1(Day12.input) should equal(1010)
  }

  it should "solve second part" in {
    Day12.part2(testInput) should equal(286)

    Day12.part2(Day12.input) should equal(52742)
  }
}
