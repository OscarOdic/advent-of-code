package com.oodic.aoc.y2017

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day02Test extends AnyFlatSpec with Matchers {
  "Day02 - 2017" should "solve first part" in {
    val testInput = List(
      List(5,1,9,5),
      List(7,5,3),
      List(2,4,6,8)
    )
    Day02.part1(testInput) should equal(18)

    Day02.part1(Day02.input) should equal(41919)
  }

  it should "solve second part" in {
    val testInput = List(
      List(5,9,2,8),
      List(9,4,7,3),
      List(3,8,6,5)
    )
    Day02.part2(testInput) should equal(9)

    Day02.part2(Day02.input) should equal(303)
  }
}
