package com.oodic.aoc.y2018

import org.scalatest.{FlatSpec, Matchers}

class Day01Test extends FlatSpec with Matchers {
  "Day01 - 2018" should "answer first part" in {
    Day01.resolveFirst(List("+1", "+1", "+1")) should be(3)
    Day01.resolveFirst(List("+1", "+1", "-2")) should be(0)
    Day01.resolveFirst(List("-1", "-2", "-3")) should be(-6)

    Day01.resolveFirst(Day01.input) should be(592)
  }

  it should "answer second part" in {
    Day01.resolveSecond(List("+1", "-1")) should be(0)
    Day01.resolveSecond(List("+3", "+3", "+4", "-2", "-4")) should be(10)
    Day01.resolveSecond(List("-6", "+3", "+8", "+5", "-6")) should be(5)
    Day01.resolveSecond(List("+7", "+7", "-2", "-7", "-4")) should be(14)

    Day01.resolveSecond(Day01.input) should be(241)
  }
}
