package com.oodic.aoc.y2022

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day04Test extends AnyFlatSpec with Matchers {
  private val testInput = List(
    "2-4,6-8",
    "2-3,4-5",
    "5-7,7-9",
    "2-8,3-7",
    "6-6,4-6",
    "2-6,4-8"
  )

  "Day04 - 2022" should "solve first part" in {
    Day04.part1(testInput) should equal(2)

    Day04.part1(Day04.input) should equal(459)
  }

  it should "solve second part" in {
    Day04.part2(testInput) should equal(4)

    Day04.part2(Day04.input) should equal(779)
  }
}
