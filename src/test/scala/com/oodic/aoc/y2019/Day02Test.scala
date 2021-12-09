package com.oodic.aoc.y2019

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day02Test extends AnyFlatSpec with Matchers {
  "Day02 - 2019" should "solve first part" in {
    Day02.part1(Day02.input) should equal(4090701)
  }

  it should "solve second part" in {
    Day02.part2(Day02.input) should equal(6421)
  }
}
