package com.oodic.aoc.y2017

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day20Test extends AnyFlatSpec with Matchers {
  "Day20 - 2017" should "solve first part" in {
    val testInput = List(
      "p=<3,0,0>, v=<2,0,0>, a=<-1,0,0>",
      "p=<4,0,0>, v=<0,0,0>, a=<-2,0,0>"
    )

    Day20.part1(testInput) should equal(0)

    Day20.part1(Day20.input) should equal(157)
  }

  it should "solve second part" in {
    val testInput = List(
      "p=<-6,0,0>, v=< 3,0,0>, a=< 0,0,0>",
      "p=<-4,0,0>, v=< 2,0,0>, a=< 0,0,0>",
      "p=<-2,0,0>, v=< 1,0,0>, a=< 0,0,0>",
      "p=< 3,0,0>, v=<-1,0,0>, a=< 0,0,0>"
    )

    Day20.part2(testInput) should equal(1)

    Day20.part2(Day20.input) should equal(499)
  }
}