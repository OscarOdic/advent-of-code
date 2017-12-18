package com.oodic.aoc

import org.scalatest.{FlatSpec, Matchers}

class Day18Test extends FlatSpec with Matchers {
  it should "answer first part" in {
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

    Day18.resolveFirst(testInput) should be(4)

    Day18.resolveFirst(Day18.input) should be(2951)
  }

  it should "answer second part" in {
    val testInput = Vector(
      "snd 1",
      "snd 2",
      "snd p",
      "rcv a",
      "rcv b",
      "rcv c",
      "rcv d"
    )

    Day18.resolveSecond(testInput) should be(3)

    Day18.resolveSecond(Day18.input) should be(7366)
  }
}