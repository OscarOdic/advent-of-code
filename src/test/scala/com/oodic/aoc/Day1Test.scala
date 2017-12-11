package com.oodic.aoc

import org.scalatest.{FlatSpec, Matchers}

class Day1Test extends FlatSpec with Matchers {
  it should "answer first part" in {
    Day1.resolveFirst(List(1,1,2,2)) should be(3)
    Day1.resolveFirst(List(1,1,1,1)) should be(4)
    Day1.resolveFirst(List(1,2,3,4)) should be(0)
    Day1.resolveFirst(List(9,1,2,1,2,1,2,9)) should be(9)

    Day1.resolveFirst(Day1.input) should be(1203)
  }

  it should "answer second part" in {
    Day1.resolveSecond(List(1,2,1,2)) should be(6)
    Day1.resolveSecond(List(1,2,2,1)) should be(0)
    Day1.resolveSecond(List(1,2,3,4,2,5)) should be(4)
    Day1.resolveSecond(List(1,2,3,1,2,3)) should be(12)
    Day1.resolveSecond(List(1,2,1,3,1,4,1,5)) should be(4)

    Day1.resolveSecond(Day1.input) should be(1146)
  }
}
