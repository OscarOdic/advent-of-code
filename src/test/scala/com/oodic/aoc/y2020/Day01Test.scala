package com.oodic.aoc.y2020

import org.scalatest.{FlatSpec, Matchers}

class Day01Test extends FlatSpec with Matchers {
  val testInput = List(
    1721,
    979,
    366,
    299,
    675,
    1456
  )

  "Day01 - 2020" should "answer first part" in {
    Day01.resolveFirst(testInput) should be(514579)

    Day01.resolveFirst(Day01.input) should be(211899)
  }

  it should "answer second part" in {
    Day01.resolveSecond(testInput) should be(241861950)

    Day01.resolveSecond(Day01.input) should be(275765682)
  }
}
