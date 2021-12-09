package com.oodic.aoc.y2020

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day08Test extends AnyFlatSpec with Matchers {
  val testInput = Vector(
    "nop +0",
    "acc +1",
    "jmp +4",
    "acc +3",
    "jmp -3",
    "acc -99",
    "acc +1",
    "jmp -4",
    "acc +6"
  )

  "Day08 - 2020" should "solve first part" in {
    Day08.part1(testInput) should equal(5)

    Day08.part1(Day08.input) should equal(2051)
  }

  it should "solve second part" in {
    Day08.part2(testInput) should equal(8)

    Day08.part2(Day08.input) should equal(2304)
  }
}
