package com.oodic.aoc.y2018

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day01Test extends AnyFlatSpec with Matchers {
  "Day01 - 2018" should "solve first part" in {
    Day01.part1(List("+1", "+1", "+1")) should equal(3)
    Day01.part1(List("+1", "+1", "-2")) should equal(0)
    Day01.part1(List("-1", "-2", "-3")) should equal(-6)

    Day01.part1(Day01.input) should equal(592)
  }

  it should "solve second part" in {
    Day01.part2(List("+1", "-1")) should equal(0)
    Day01.part2(List("+3", "+3", "+4", "-2", "-4")) should equal(10)
    Day01.part2(List("-6", "+3", "+8", "+5", "-6")) should equal(5)
    Day01.part2(List("+7", "+7", "-2", "-7", "-4")) should equal(14)

    Day01.part2(Day01.input) should equal(241)
  }
}
