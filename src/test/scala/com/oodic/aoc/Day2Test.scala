package com.oodic.aoc

import org.scalatest.{FlatSpec, Matchers}

class Day2Test extends FlatSpec with Matchers {
  it should "answer first part" in {
    val testInput = List(
      List(5,1,9,5),
      List(7,5,3),
      List(2,4,6,8)
    )
    Day2.resolveFirst(testInput) should be(18)

    Day2.resolveFirst(Day2.input) should be(41919)
  }

  it should "answer second part" in {
    val testInput = List(
      List(5,9,2,8),
      List(9,4,7,3),
      List(3,8,6,5)
    )
    Day2.resolveSecond(testInput) should be(9)

    Day2.resolveSecond(Day2.input) should be(303)
  }
}
