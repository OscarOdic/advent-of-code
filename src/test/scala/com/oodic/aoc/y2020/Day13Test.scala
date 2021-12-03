package com.oodic.aoc.y2020

import org.scalatest.{FlatSpec, Matchers}

class Day13Test extends FlatSpec with Matchers {
  val testInput = (
    939,
    List("7","13","x","x","59","x","31","19")
  )


  "Day13 - 2020" should "answer first part" in {
    Day13.resolveFirst(testInput) should be(295)

    Day13.resolveFirst(Day13.input) should be(174)
  }

  it should "answer second part" in {
    Day13.resolveSecond(testInput) should be(1068781L)
    Day13.resolveSecond((0, List("17","x","13","19"))) should be(3417L)
    Day13.resolveSecond((0, List("67","7","59","61"))) should be(754018L)
    Day13.resolveSecond((0, List("67","x","7","59","61"))) should be(779210L)
    Day13.resolveSecond((0, List("67","7","x","59","61"))) should be(1261476L)
    Day13.resolveSecond((0, List("1789","37","47","1889"))) should be(1202161486L)

    Day13.resolveSecond(Day13.input) should be(780601154795940L)
  }
}
