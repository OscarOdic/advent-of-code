package com.oodic.aoc.y2017

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day12Test extends AnyFlatSpec with Matchers {
  val testInput = List(
    "0 <-> 2",
    "1 <-> 1",
    "2 <-> 0, 3, 4",
    "3 <-> 2, 4",
    "4 <-> 2, 3, 6",
    "5 <-> 6",
    "6 <-> 4, 5"
  )

  "Day12 - 2017" should "solve first part" in {
    Day12.part1(testInput) should equal(6)

    Day12.part1(Day12.input) should equal(306)
  }

  it should "solve second part" in {
    Day12.part2(testInput) should equal(2)

    Day12.part2(Day12.input) should equal(200)
  }
}
