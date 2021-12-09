package com.oodic.aoc.y2020

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day10Test extends AnyFlatSpec with Matchers {
  val testInput1 = List(
    16,
    10,
    15,
    5,
    1,
    11,
    7,
    19,
    6,
    12,
    4
  )

  val testInput2 = List(
    28,
    33,
    18,
    42,
    31,
    14,
    46,
    20,
    48,
    47,
    24,
    23,
    49,
    45,
    19,
    38,
    39,
    11,
    1,
    32,
    25,
    35,
    8,
    17,
    7,
    9,
    4,
    2,
    34,
    10,
    3
  )

  "Day10 - 2020" should "solve first part" in {
    Day10.part1(testInput1) should equal(35)
    Day10.part1(testInput2) should equal(220)

    Day10.part1(Day10.input) should equal(1755)
  }

  it should "solve second part" in {
    Day10.part2(testInput1) should equal(8L)
    Day10.part2(testInput2) should equal(19208L)

    Day10.part2(Day10.input) should equal(4049565169664L)
  }
}
