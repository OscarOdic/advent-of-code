package com.oodic.aoc.y2020

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day01Test extends AnyFlatSpec with Matchers {
  val testInput = List(
    1721,
    979,
    366,
    299,
    675,
    1456
  )

  "Day01 - 2020" should "solve first part" in {
    Day01.part1(testInput) should equal(514579)

    Day01.part1(Day01.input) should equal(211899)
  }

  it should "solve second part" in {
    Day01.part2(testInput) should equal(241861950)

    Day01.part2(Day01.input) should equal(275765682)
  }
}
