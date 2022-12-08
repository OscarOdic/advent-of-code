package com.oodic.aoc.y2022

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day08Test extends AnyFlatSpec with Matchers {
  private val testInput = Day08.toGrid(List(
    "30373",
    "25512",
    "65332",
    "33549",
    "35390",
  ))

  "Day08 - 2022" should "solve first part" in {
    Day08.part1(testInput) should equal(21)

    Day08.part1(Day08.input) should equal(1779)
  }

  it should "solve second part" in {
    Day08.part2(testInput) should equal(8)

    Day08.part2(Day08.input) should equal(172224)
  }
}
