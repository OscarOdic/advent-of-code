package com.oodic.aoc.y2021

import org.scalatest.{FlatSpec, Matchers}

class Day07Test extends FlatSpec with Matchers {
  val testInput = List(
    16,1,2,0,4,2,7,1,2,14
  )

  "Day07 - 2021" should "answer first part" in {
    Day07.resolveFirst(testInput) should be(37)

    Day07.resolveFirst(Day07.input) should be(344535)
  }

  it should "answer second part" in {
    Day07.resolveSecond(testInput) should be(168)

    Day07.resolveSecond(Day07.input) should be(95581659)
  }
}
