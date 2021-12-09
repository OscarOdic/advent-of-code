package com.oodic.aoc.y2017

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day21Test extends AnyFlatSpec with Matchers {
  "Day21 - 2017" should "solve first part" in {
    val testInput = List(
      "../.# => ##./#../...",
      ".#./..#/### => #..#/..../..../#..#"
    )

    Day21.resolve(testInput, 2) should equal(12)

    Day21.part1(Day21.input) should equal(188)
  }

  it should "solve second part" in {
    Day21.part2(Day21.input) should equal(2758764)
  }
}