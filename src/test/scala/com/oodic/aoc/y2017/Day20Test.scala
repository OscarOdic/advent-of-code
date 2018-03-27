package com.oodic.aoc.y2017

import org.scalatest.{FlatSpec, Matchers}

class Day20Test extends FlatSpec with Matchers {
  "Day20 - 2017" should "answer first part" in {
    val testInput = List(
      "p=<3,0,0>, v=<2,0,0>, a=<-1,0,0>",
      "p=<4,0,0>, v=<0,0,0>, a=<-2,0,0>"
    )

    Day20.resolveFirst(testInput) should be(0)

    Day20.resolveFirst(Day20.input) should be(157)
  }

  it should "answer second part" in {
    val testInput = List(
      "p=<-6,0,0>, v=< 3,0,0>, a=< 0,0,0>",
      "p=<-4,0,0>, v=< 2,0,0>, a=< 0,0,0>",
      "p=<-2,0,0>, v=< 1,0,0>, a=< 0,0,0>",
      "p=< 3,0,0>, v=<-1,0,0>, a=< 0,0,0>"
    )

    Day20.resolveSecond(testInput) should be(1)

    Day20.resolveSecond(Day20.input) should be(499)
  }
}