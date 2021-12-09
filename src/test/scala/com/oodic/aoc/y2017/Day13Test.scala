package com.oodic.aoc.y2017

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day13Test extends AnyFlatSpec with Matchers {
  val testInput = List(
    "0: 3",
    "1: 2",
    "4: 4",
    "6: 4"
  )

  "Day13 - 2017" should "solve first part" in {
    Day13.part1(testInput) should equal(24)

    Day13.part1(Day13.input) should equal(632)
  }

  it should "solve second part" in {
    Day13.part2(testInput) should equal(10)

    Day13.part2(Day13.input) should equal(3849742)
  }
}
