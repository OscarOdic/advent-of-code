package com.oodic.aoc.y2020

import org.scalatest.{FlatSpec, Matchers}

class Day08Test extends FlatSpec with Matchers {
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

  "Day08 - 2020" should "answer first part" in {
    Day08.resolveFirst(testInput) should be(5)

    Day08.resolveFirst(Day08.input) should be(2051)
  }

  it should "answer second part" in {
    Day08.resolveSecond(testInput) should be(8)

    Day08.resolveSecond(Day08.input) should be(2304)
  }
}
