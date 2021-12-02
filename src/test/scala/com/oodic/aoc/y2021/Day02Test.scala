package com.oodic.aoc.y2021

import org.scalatest.{FlatSpec, Matchers}

class Day02Test extends FlatSpec with Matchers {
  val testInput = List(
    "forward 5",
    "down 5",
    "forward 8",
    "up 3",
    "down 8",
    "forward 2",
  )

  "Day02 - 2021" should "answer first part" in {
    Day02.resolveFirst(testInput) should be(150)

    Day02.resolveFirst(Day02.input) should be(2215080)
  }

  it should "answer second part" in {
    Day02.resolveSecond(testInput) should be(900)

    Day02.resolveSecond(Day02.input) should be(1864715580)
  }
}
