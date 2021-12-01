package com.oodic.aoc.y2020

import org.scalatest.{FlatSpec, Matchers}

class Day02Test extends FlatSpec with Matchers {
  val testInput = List(
    "1-3 a: abcde",
    "1-3 b: cdefg",
    "2-9 c: ccccccccc"
  )

  "Day02 - 2020" should "answer first part" in {
    Day02.resolveFirst(testInput) should be(2)

    Day02.resolveFirst(Day02.input) should be(572)
  }

  it should "answer second part" in {
    Day02.resolveSecond(testInput) should be(1)

    Day02.resolveSecond(Day02.input) should be(306)
  }
}
