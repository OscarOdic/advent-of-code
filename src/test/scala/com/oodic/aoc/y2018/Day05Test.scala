package com.oodic.aoc.y2018

import org.scalatest.{FlatSpec, Matchers}

class Day05Test extends FlatSpec with Matchers {
  val testInput = "dabAcCaCBAcCcaDA"

  "Day05 - 2018" should "answer first part" in {
    Day05.resolveFirst(testInput) should be(10)

    Day05.resolveFirst(Day05.input) should be(9900)
  }

  it should "answer second part" in {
    Day05.resolveSecond(testInput) should be(4)

    Day05.resolveSecond(Day05.input) should be(4992)
  }
}
