package com.oodic.aoc.y2017

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day17Test extends AnyFlatSpec with Matchers {
  "Day17 - 2017" should "solve first part" in {
    Day17.part1(3) should equal(638)

    Day17.part1(Day17.input) should equal(1244)
  }

  it should "solve second part" in {
    Day17.part2(Day17.input) should equal(11162912)
  }
}