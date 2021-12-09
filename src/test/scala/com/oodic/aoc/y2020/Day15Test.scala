package com.oodic.aoc.y2020

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day15Test extends AnyFlatSpec with Matchers {
  val testInput = List(0, 3, 6)

  "Day15 - 2020" should "solve first part" in {
    Day15.part1(testInput) should equal(436)

    Day15.part1(Day15.input) should equal(441)
  }

  it should "solve second part" in {
    Day15.part2(testInput) should equal(175594)
    Day15.part2(List(1, 3, 2)) should equal(2578)
    Day15.part2(List(2, 1, 3)) should equal(3544142)
    Day15.part2(List(1, 2, 3)) should equal(261214)
    Day15.part2(List(2, 3, 1)) should equal(6895259)
    Day15.part2(List(3, 2, 1)) should equal(18)
    Day15.part2(List(3, 1, 2)) should equal(362)

    Day15.part2(Day15.input) should equal(10613991)
  }
}
