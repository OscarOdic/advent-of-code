package com.oodic.aoc.y2016

import org.scalatest.{FlatSpec, Matchers}

class Day01Test extends FlatSpec with Matchers {
  "Day01 - 2016" should "answer first part" in {
    Day01.resolveFirst(List("R2", "L3")) should be(5)
    Day01.resolveFirst(List("R2", "R2", "R2")) should be(2)
    Day01.resolveFirst(List("R5", "L5", "R5", "R3")) should be(12)

    Day01.resolveFirst(Day01.input) should be(241)
  }

  it should "answer second part" in {
    Day01.resolveSecond(List("R8", "R4", "R4", "R8")) should be(4)

    Day01.resolveSecond(Day01.input) should be(116)
  }
}
