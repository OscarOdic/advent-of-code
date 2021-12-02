package com.oodic.aoc.y2020

import org.scalatest.{FlatSpec, Matchers}

class Day06Test extends FlatSpec with Matchers {
  val testInput = List(
    List("abc"),

    List("a",
      "b",
      "c"),

    List("ab",
      "ac"),

    List("a",
      "a",
      "a",
      "a"),

    List("b")
  )

  "Day06 - 2020" should "answer first part" in {
    Day06.resolveFirst(testInput) should be(11)

    Day06.resolveFirst(Day06.input) should be(6565)
  }

  it should "answer second part" in {
    Day06.resolveSecond(testInput) should be(6)

    Day06.resolveSecond(Day06.input) should be(3137)
  }
}
