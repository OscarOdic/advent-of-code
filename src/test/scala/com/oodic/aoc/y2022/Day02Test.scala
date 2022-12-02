package com.oodic.aoc.y2022

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day02Test extends AnyFlatSpec with Matchers {
  val testInput = List(
    ("A", "Y"),
    ("B", "X"),
    ("C", "Z")
  )

  "Day02 - 2021" should "solve first part" in {
    Day02.part1(testInput) should equal(15)

    Day02.part1(Day02.input) should equal(14375)
  }

  it should "solve second part" in {
    Day02.part2(testInput) should equal(12)

    Day02.part2(Day02.input) should equal(10274)
  }
}
