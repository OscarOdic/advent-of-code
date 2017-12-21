package com.oodic.aoc

import org.scalatest.{FlatSpec, Matchers}

class Day21Test extends FlatSpec with Matchers {
  it should "answer first part" in {
    val testInput = List(
      "../.# => ##./#../...",
      ".#./..#/### => #..#/..../..../#..#"
    )

    Day21.resolve(testInput, 2) should be(12)

    Day21.resolveFirst(Day21.input) should be(188)
  }

  it should "answer second part" in {
    Day21.resolveSecond(Day21.input) should be(2758764)
  }
}