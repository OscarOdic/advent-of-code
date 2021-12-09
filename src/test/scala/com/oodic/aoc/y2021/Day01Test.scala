package com.oodic.aoc.y2021

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day01Test extends AnyFlatSpec with Matchers {
  val testInput = List(
    199,
    200,
    208,
    210,
    200,
    207,
    240,
    269,
    260,
    263
  )

  "Day01 - 2021" should "solve first part" in {
    Day01.part1(testInput) should equal(7)

    Day01.part1(Day01.input) should equal(1529)
  }

  it should "solve second part" in {
    Day01.part2(testInput) should equal(5)

    Day01.part2(Day01.input) should equal(1567)
  }
}
