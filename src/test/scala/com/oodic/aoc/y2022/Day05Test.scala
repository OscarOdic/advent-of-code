package com.oodic.aoc.y2022

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day05Test extends AnyFlatSpec with Matchers {
  private val testInput = (
    List(
      "    [D]    ",
      "[N] [C]    ",
      "[Z] [M] [P]",
      " 1   2   3 "
    ),
    List(
      "move 1 from 2 to 1",
      "move 3 from 1 to 3",
      "move 2 from 2 to 1",
      "move 1 from 1 to 2"
    )
  )

  "Day05 - 2022" should "solve first part" in {
    Day05.part1(testInput) should equal("CMZ")

    Day05.part1(Day05.input) should equal("PTWLTDSJV")
  }

  it should "solve second part" in {
    Day05.part2(testInput) should equal("MCD")

    Day05.part2(Day05.input) should equal("WZMFVGGZP")
  }
}
