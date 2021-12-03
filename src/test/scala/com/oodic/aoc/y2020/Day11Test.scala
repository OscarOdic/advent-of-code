package com.oodic.aoc.y2020

import org.scalatest.{FlatSpec, Matchers}

class Day11Test extends FlatSpec with Matchers {
  val testInput1 = (for {
    (value, y) <- List(
      "L.LL.LL.LL",
      "LLLLLLL.LL",
      "L.L.L..L..",
      "LLLL.LL.LL",
      "L.LL.LL.LL",
      "L.LLLLL.LL",
      "..L.L.....",
      "LLLLLLLLLL",
      "L.LLLLLL.L",
      "L.LLLLL.LL"
    ).zipWithIndex
    (char, x) <- value.zipWithIndex
  } yield ((x, y), char)
    ).toMap


  "Day11 - 2020" should "answer first part" in {
    Day11.resolveFirst(testInput1) should be(37)

    Day11.resolveFirst(Day11.input) should be(2251)
  }

  it should "answer second part" in {
    Day11.resolveSecond(testInput1) should be(26)

    Day11.resolveSecond(Day11.input) should be(2019)
  }
}
