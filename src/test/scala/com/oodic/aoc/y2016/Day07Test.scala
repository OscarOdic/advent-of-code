package com.oodic.aoc.y2016

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day07Test extends AnyFlatSpec with Matchers {
  "Day07 - 2016" should "solve first part" in {
    val testInput: List[String] = List(
      "abba[mnop]qrst",
      "abcd[bddb]xyyx",
      "aaaa[qwer]tyui",
      "ioxxoj[asdfgh]zxcvbn"
    )

    Day07.part1(testInput) should equal(2)

    Day07.part1(Day07.input) should equal(115)
  }

  it should "solve second part" in {
    val testInput: List[String] = List(
      "aba[bab]xyz",
      "xyx[xyx]xyx",
      "aaa[kek]eke",
      "zazbz[bzb]cdb"
    )

    Day07.part2(testInput) should equal(3)

    Day07.part2(Day07.input) should equal(231)
  }
}
