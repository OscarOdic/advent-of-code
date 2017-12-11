package com.oodic.aoc

import org.scalatest.{FlatSpec, Matchers}

class Day11Test extends FlatSpec with Matchers {
  it should "answer first part" in {
    Day11.resolveFirst(List("ne","ne","ne")) should be(3)
    Day11.resolveFirst(List("ne","ne","sw","sw")) should be(0)
    Day11.resolveFirst(List("ne","ne","s","s")) should be(2)
    Day11.resolveFirst(List("se","sw","se","sw","sw")) should be(3)

    Day11.resolveFirst(Day11.input) should be(759)
  }

  it should "answer second part" in {
    Day11.resolveSecond(Day11.input) should be(1501)
  }
}
