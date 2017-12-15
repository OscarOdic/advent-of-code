package com.oodic.aoc

import org.scalatest.{FlatSpec, Matchers}

class Day15Test extends FlatSpec with Matchers {
  val testInput = (65L, 8921L)

  it should "answer first part" in {
    Day15.resolveFirst(testInput) should be(588)

    Day15.resolveFirst(Day15.input) should be(650)
  }

  it should "answer second part" in {
    Day15.resolveSecond(testInput) should be(309)

    Day15.resolveSecond(Day15.input) should be(336)
  }
}
