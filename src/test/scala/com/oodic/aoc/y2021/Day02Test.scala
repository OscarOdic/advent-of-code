package com.oodic.aoc.y2021

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day02Test extends AnyFlatSpec with Matchers {
  val testInput = List(
    "forward 5",
    "down 5",
    "forward 8",
    "up 3",
    "down 8",
    "forward 2",
  )

  "Day02 - 2021" should "solve first part" in {
    Day02.part1(testInput) should equal(150)

    Day02.part1(Day02.input) should equal(2215080)
  }

  it should "solve second part" in {
    Day02.part2(testInput) should equal(900)

    Day02.part2(Day02.input) should equal(1864715580)
  }
}
