package com.oodic.aoc

import org.scalatest.{FlatSpec, Matchers}

class Day01Test extends FlatSpec with Matchers {
  it should "answer first part" in {
    Day01.resolveFirst(List(1,1,2,2)) should be(3)
    Day01.resolveFirst(List(1,1,1,1)) should be(4)
    Day01.resolveFirst(List(1,2,3,4)) should be(0)
    Day01.resolveFirst(List(9,1,2,1,2,1,2,9)) should be(9)

    Day01.resolveFirst(Day01.input) should be(1203)
  }

  it should "answer second part" in {
    Day01.resolveSecond(List(1,2,1,2)) should be(6)
    Day01.resolveSecond(List(1,2,2,1)) should be(0)
    Day01.resolveSecond(List(1,2,3,4,2,5)) should be(4)
    Day01.resolveSecond(List(1,2,3,1,2,3)) should be(12)
    Day01.resolveSecond(List(1,2,1,3,1,4,1,5)) should be(4)

    Day01.resolveSecond(Day01.input) should be(1146)
  }
}
