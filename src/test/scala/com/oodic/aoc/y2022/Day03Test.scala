package com.oodic.aoc.y2022

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day03Test extends AnyFlatSpec with Matchers {
  private val testInput = List(
    "vJrwpWtwJgWrhcsFMMfFFhFp",
    "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL",
    "PmmdzqPrVvPwwTWBwg",
    "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn",
    "ttgJtRGJQctTZtZT",
    "CrZsJsPPZsGzwwsLwLmpwMDw"
  )

  "Day03 - 2022" should "solve first part" in {
    Day03.part1(testInput) should equal(157)

    Day03.part1(Day03.input) should equal(8088)
  }

  it should "solve second part" in {
    Day03.part2(testInput) should equal(70)

    Day03.part2(Day03.input) should equal(2522)
  }
}
