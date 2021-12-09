package com.oodic.aoc.y2020

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day13Test extends AnyFlatSpec with Matchers {
  val testInput = (
    939,
    List("7","13","x","x","59","x","31","19")
  )


  "Day13 - 2020" should "solve first part" in {
    Day13.part1(testInput) should equal(295)

    Day13.part1(Day13.input) should equal(174)
  }

  it should "solve second part" in {
    Day13.part2(testInput) should equal(1068781L)
    Day13.part2((0, List("17","x","13","19"))) should equal(3417L)
    Day13.part2((0, List("67","7","59","61"))) should equal(754018L)
    Day13.part2((0, List("67","x","7","59","61"))) should equal(779210L)
    Day13.part2((0, List("67","7","x","59","61"))) should equal(1261476L)
    Day13.part2((0, List("1789","37","47","1889"))) should equal(1202161486L)

    Day13.part2(Day13.input) should equal(780601154795940L)
  }
}
