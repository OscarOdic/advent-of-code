package com.oodic.aoc.y2017

import org.scalatest.{FlatSpec, Matchers}

class Day24Test extends FlatSpec with Matchers {
  val testInput = Vector(
    "0/2",
    "2/2",
    "2/3",
    "3/4",
    "3/5",
    "0/1",
    "10/1",
    "9/10"
  )

  "Day24 - 2017" should "answer first part" in {
    Day24.resolveFirst(testInput) should be(31)

    Day24.resolveFirst(Day24.input) should be(1940)
  }

  it should "answer second part" in {
    Day24.resolveSecond(Day24.input) should be(1928)
  }
}