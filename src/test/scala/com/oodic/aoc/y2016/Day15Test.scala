package com.oodic.aoc.y2016

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day15Test extends AnyFlatSpec with Matchers {
  val testInput: List[String] = List(
    "Disc #1 has 5 positions; at time=0, it is at position 4.",
    "Disc #2 has 2 positions; at time=0, it is at position 1."
  )

  "Day15 - 2016" should "solve first part" in {
    Day15.part1(testInput) should equal(5)

    Day15.part1(Day15.input) should equal(148737)
  }

  it should "solve second part" in {
    Day15.part2(Day15.input) should equal(2353212)
  }
}
