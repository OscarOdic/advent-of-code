package com.oodic.aoc.y2020

import org.scalatest.{FlatSpec, Matchers}

class Day18Test extends FlatSpec with Matchers {
  val testInput = List(
    "2 * 3 + (4 * 5)",
    "5 + (8 * 3 + 9 + 3 * 4 * 3)",
    "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))",
    "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"
  )

  "Day18 - 2020" should "answer first part" in {
    Day18.resolveFirst(testInput) should be(26335)

    Day18.resolveFirst(Day18.input) should be(67800526776934L)
  }

  it should "answer second part" in {

    Day18.resolveSecond(testInput) should be(693891)

    Day18.resolveSecond(Day18.input) should be(340789638435483L)
  }
}
