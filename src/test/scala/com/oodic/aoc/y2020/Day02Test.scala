package com.oodic.aoc.y2020

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day02Test extends AnyFlatSpec with Matchers {
  val testInput = List(
    "1-3 a: abcde",
    "1-3 b: cdefg",
    "2-9 c: ccccccccc"
  )

  "Day02 - 2020" should "solve first part" in {
    Day02.part1(testInput) should equal(2)

    Day02.part1(Day02.input) should equal(572)
  }

  it should "solve second part" in {
    Day02.part2(testInput) should equal(1)

    Day02.part2(Day02.input) should equal(306)
  }
}
