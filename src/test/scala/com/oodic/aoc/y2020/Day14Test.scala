package com.oodic.aoc.y2020

import org.scalatest.{FlatSpec, Matchers}

class Day14Test extends FlatSpec with Matchers {
  "Day14 - 2020" should "answer first part" in {
    val testInput = List(
      "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X",
      "mem[8] = 11",
      "mem[7] = 101",
      "mem[8] = 0"
    )

    Day14.resolveFirst(testInput) should be(165L)

    Day14.resolveFirst(Day14.input) should be(12512013221615L)
  }

  it should "answer second part" in {
    val testInput = List(
      "mask = 000000000000000000000000000000X1001X",
      "mem[42] = 100",
      "mask = 00000000000000000000000000000000X0XX",
      "mem[26] = 1"
    )

    Day14.resolveSecond(testInput) should be(208L)

    Day14.resolveSecond(Day14.input) should be(3905642473893L)
  }
}
