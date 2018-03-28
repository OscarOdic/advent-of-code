package com.oodic.aoc.y2016

import org.scalatest.{FlatSpec, Matchers}

class Day04Test extends FlatSpec with Matchers {
  val testInput: List[String] = List(
    "aaaaa-bbb-z-y-x-123[abxyz]",
    "a-b-c-d-e-f-g-h-987[abcde]",
    "not-a-real-room-404[oarel]",
    "totally-real-room-200[decoy]"
  )

  "Day04 - 2016" should "answer first part" in {
    Day04.resolveFirst(testInput) should be(1514)

    Day04.resolveFirst(Day04.input) should be(278221)
  }

  it should "answer second part" in {
    Day04.resolveSecond(Day04.input) should be(267)
  }
}
