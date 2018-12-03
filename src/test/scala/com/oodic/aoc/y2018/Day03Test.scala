package com.oodic.aoc.y2018

import org.scalatest.{FlatSpec, Matchers}

class Day03Test extends FlatSpec with Matchers {
  val testInput = List(
    "#1 @ 1,3: 4x4",
    "#2 @ 3,1: 4x4",
    "#3 @ 5,5: 2x2"
  )

  "Day03 - 2018" should "answer first part" in {
    Day03.resolveFirst(testInput) should be(4)

    Day03.resolveFirst(Day03.input) should be(112418)
  }

  it should "answer second part" in {
    Day03.resolveSecond(testInput) should be(3)

    Day03.resolveSecond(Day03.input) should be(560)
  }
}
