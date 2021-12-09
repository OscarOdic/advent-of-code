package com.oodic.aoc.y2016

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day12Test extends AnyFlatSpec with Matchers {
  val testInput: List[String] = List(
    "cpy 41 a",
    "inc a",
    "inc a",
    "dec a",
    "jnz a 2",
    "dec a"
  )

  "Day12 - 2016" should "solve first part" in {
    Day12.part1(testInput) should equal(42)

    Day12.part1(Day12.input) should equal(318009)
  }

  it should "solve second part" in {
    Day12.part2(Day12.input) should equal(9227663)
  }
}
