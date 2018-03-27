package com.oodic.aoc.y2017

import org.scalatest.{FlatSpec, Matchers}

class Day02Test extends FlatSpec with Matchers {
  "Day02 - 2017" should "answer first part" in {
    val testInput = List(
      List(5,1,9,5),
      List(7,5,3),
      List(2,4,6,8)
    )
    Day02.resolveFirst(testInput) should be(18)

    Day02.resolveFirst(Day02.input) should be(41919)
  }

  it should "answer second part" in {
    val testInput = List(
      List(5,9,2,8),
      List(9,4,7,3),
      List(3,8,6,5)
    )
    Day02.resolveSecond(testInput) should be(9)

    Day02.resolveSecond(Day02.input) should be(303)
  }
}
