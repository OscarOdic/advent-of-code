package com.oodic.aoc.y2016

import org.scalatest.{FlatSpec, Matchers}

class Day07Test extends FlatSpec with Matchers {
  "Day07 - 2016" should "answer first part" in {
    val testInput: List[String] = List(
      "abba[mnop]qrst",
      "abcd[bddb]xyyx",
      "aaaa[qwer]tyui",
      "ioxxoj[asdfgh]zxcvbn"
    )

    Day07.resolveFirst(testInput) should be(2)

    Day07.resolveFirst(Day07.input) should be(115)
  }

  it should "answer second part" in {
    val testInput: List[String] = List(
      "aba[bab]xyz",
      "xyx[xyx]xyx",
      "aaa[kek]eke",
      "zazbz[bzb]cdb"
    )

    Day07.resolveSecond(testInput) should be(3)

    Day07.resolveSecond(Day07.input) should be(231)
  }
}
