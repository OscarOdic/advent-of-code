package com.oodic.aoc.y2021

import org.scalatest.{FlatSpec, Matchers}

class Day05Test extends FlatSpec with Matchers {
  val testInput = List(
    "0,9 -> 5,9",
    "8,0 -> 0,8",
    "9,4 -> 3,4",
    "2,2 -> 2,1",
    "7,0 -> 7,4",
    "6,4 -> 2,0",
    "0,9 -> 2,9",
    "3,4 -> 1,4",
    "0,0 -> 8,8",
    "5,5 -> 8,2"
  )

  "Day05 - 2021" should "answer first part" in {
    Day05.resolveFirst(testInput) should be(5)

    Day05.resolveFirst(Day05.input) should be(7468)
  }

  it should "answer second part" in {
    Day05.resolveSecond(testInput) should be(12)

    Day05.resolveSecond(Day05.input) should be(22364)
  }
}
