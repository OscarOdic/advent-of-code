package com.oodic.aoc.y2020

import org.scalatest.{FlatSpec, Matchers}

class Day10Test extends FlatSpec with Matchers {
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

  "Day10 - 2020" should "answer first part" in {
    Day10.resolveFirst(testInput1) should be(35)
    Day10.resolveFirst(testInput2) should be(220)

    Day10.resolveFirst(Day10.input) should be(1755)
  }

  it should "answer second part" in {
    Day10.resolveSecond(testInput1) should be(8L)
    Day10.resolveSecond(testInput2) should be(19208L)

    Day10.resolveSecond(Day10.input) should be(4049565169664L)
  }
}
