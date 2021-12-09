package com.oodic.aoc.y2017

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day18Test extends AnyFlatSpec with Matchers {
  "Day18 - 2017" should "solve first part" in {
    val testInput = Vector(
      "set a 1",
      "add a 2",
      "mul a a",
      "mod a 5",
      "snd a",
      "set a 0",
      "rcv a",
      "jgz a -1",
      "set a 1",
      "jgz a -2"
    )

    Day18.part1(testInput) should equal(4)

    Day18.part1(Day18.input) should equal(2951)
  }

  it should "solve second part" in {
    val testInput = Vector(
      "snd 1",
      "snd 2",
      "snd p",
      "rcv a",
      "rcv b",
      "rcv c",
      "rcv d"
    )

    Day18.part2(testInput) should equal(3)

    Day18.part2(Day18.input) should equal(7366)
  }
}