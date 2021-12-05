package com.oodic.aoc.y2020

import org.scalatest.{FlatSpec, Matchers}

class Day17Test extends FlatSpec with Matchers {
  val testInput = List(
    ".#.",
    "..#",
    "###"
  )

  "Day17 - 2020" should "answer first part" in {
    Day17.resolveFirst(testInput) should be(112)

    Day17.resolveFirst(Day17.input) should be(230)
  }

  it should "answer second part" in {
    Day17.resolveSecond(testInput) should be(848)

    Day17.resolveSecond(Day17.input) should be(1600)
  }
}
