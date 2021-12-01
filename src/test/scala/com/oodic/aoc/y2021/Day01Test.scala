package com.oodic.aoc.y2021

import org.scalatest.{FlatSpec, Matchers}

class Day01Test extends FlatSpec with Matchers {
  val testInput = List(
    199,
    200,
    208,
    210,
    200,
    207,
    240,
    269,
    260,
    263
  )

  "Day01 - 2021" should "answer first part" in {
    Day01.resolveFirst(testInput) should be(7)

    Day01.resolveFirst(Day01.input) should be(1529)
  }

  it should "answer second part" in {
    Day01.resolveSecond(testInput) should be(5)

    Day01.resolveSecond(Day01.input) should be(1567)
  }
}
