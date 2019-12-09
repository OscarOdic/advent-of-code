package com.oodic.aoc.y2019

import org.scalatest.{FlatSpec, Matchers}

class Day06Test extends FlatSpec with Matchers {
  val testInput = Vector(
    "COM)B",
    "B)C",
    "C)D",
    "D)E",
    "E)F",
    "B)G",
    "G)H",
    "D)I",
    "E)J",
    "J)K",
    "K)L"
  )

  "Day06 - 2019" should "answer first part" in {
    Day06.resolveFirst(testInput) should be (42)

    Day06.resolveFirst(Day06.input) should be(119831)
  }

  it should "answer second part" in {
    Day06.resolveSecond(testInput ++ Vector("K)YOU","I)SAN")) should be (4)

    Day06.resolveSecond(Day06.input) should be(322)
  }
}
