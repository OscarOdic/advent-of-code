package com.oodic.aoc.y2021

import org.scalatest.{FlatSpec, Matchers}

class Day03Test extends FlatSpec with Matchers {
  val testInput = List(
    List(0, 0, 1, 0, 0),
    List(1, 1, 1, 1, 0),
    List(1, 0, 1, 1, 0),
    List(1, 0, 1, 1, 1),
    List(1, 0, 1, 0, 1),
    List(0, 1, 1, 1, 1),
    List(0, 0, 1, 1, 1),
    List(1, 1, 1, 0, 0),
    List(1, 0, 0, 0, 0),
    List(1, 1, 0, 0, 1),
    List(0, 0, 0, 1, 0),
    List(0, 1, 0, 1, 0)
  )

  "Day03 - 2021" should "answer first part" in {
    Day03.resolveFirst(testInput) should be(198)

    Day03.resolveFirst(Day03.input) should be(3549854)
  }

  it should "answer second part" in {
    Day03.resolveSecond(testInput) should be(230)

    Day03.resolveSecond(Day03.input) should be(3765399)
  }
}
