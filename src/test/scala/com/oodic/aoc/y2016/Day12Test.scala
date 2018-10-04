package com.oodic.aoc.y2016

import org.scalatest.{FlatSpec, Matchers}

class Day12Test extends FlatSpec with Matchers {
  val testInput: List[String] = List(
    "cpy 41 a",
    "inc a",
    "inc a",
    "dec a",
    "jnz a 2",
    "dec a"
  )

  "Day12 - 2016" should "answer first part" in {
    Day12.resolveFirst(testInput) should be(42)

    Day12.resolveFirst(Day12.input) should be(318009)
  }

  it should "answer second part" in {
    Day12.resolveSecond(Day12.input) should be(9227663)
  }
}
