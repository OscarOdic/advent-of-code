package com.oodic.aoc.y2020

import org.scalatest.{FlatSpec, Matchers}

class Day15Test extends FlatSpec with Matchers {
  val testInput = List(0, 3, 6)

  "Day15 - 2020" should "answer first part" in {
    Day15.resolveFirst(testInput) should be(436)

    Day15.resolveFirst(Day15.input) should be(441)
  }

  it should "answer second part" in {
    Day15.resolveSecond(testInput) should be(175594)
    Day15.resolveSecond(List(1, 3, 2)) should be(2578)
    Day15.resolveSecond(List(2, 1, 3)) should be(3544142)
    Day15.resolveSecond(List(1, 2, 3)) should be(261214)
    Day15.resolveSecond(List(2, 3, 1)) should be(6895259)
    Day15.resolveSecond(List(3, 2, 1)) should be(18)
    Day15.resolveSecond(List(3, 1, 2)) should be(362)

    Day15.resolveSecond(Day15.input) should be(10613991)
  }
}
