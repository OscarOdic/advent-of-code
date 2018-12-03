package com.oodic.aoc.y2018

import org.scalatest.{FlatSpec, Matchers}

class Day02Test extends FlatSpec with Matchers {
  "Day02 - 2018" should "answer first part" in {
    val testInput = List(
      "abcdef",
      "bababc",
      "abbcde",
      "abcccd",
      "aabcdd",
      "abcdee",
      "ababab"
    )

    Day02.resolveFirst(testInput) should be(12)

    Day02.resolveFirst(Day02.input) should be(8820)
  }

  it should "answer second part" in {
    val testInput = List(
      "abcde",
      "fghij",
      "klmno",
      "pqrst",
      "fguij",
      "axcye",
      "wvxyz"
    )

    Day02.resolveSecond(testInput) should be("fgij")

    Day02.resolveSecond(Day02.input) should be("bpacnmglhizqygfsjixtkwudr")
  }
}
