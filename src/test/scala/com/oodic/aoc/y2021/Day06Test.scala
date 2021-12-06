package com.oodic.aoc.y2021

import org.scalatest.{FlatSpec, Matchers}

class Day06Test extends FlatSpec with Matchers {
  val testInput: List[Long] = List(
    3,4,3,1,2
  )

  "Day06 - 2021" should "answer first part" in {
    Day06.resolveFirst(testInput) should be(5934)

    Day06.resolveFirst(Day06.input) should be(371379)
  }

  it should "answer second part" in {
    Day06.resolveSecond(testInput) should be(26984457539L)

    Day06.resolveSecond(Day06.input) should be(1674303997472L)
  }
}
