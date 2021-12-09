package com.oodic.aoc.y2021

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day03Test extends AnyFlatSpec with Matchers {
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

  "Day03 - 2021" should "solve first part" in {
    Day03.part1(testInput) should equal(198)

    Day03.part1(Day03.input) should equal(3549854)
  }

  it should "solve second part" in {
    Day03.part2(testInput) should equal(230)

    Day03.part2(Day03.input) should equal(3765399)
  }
}
