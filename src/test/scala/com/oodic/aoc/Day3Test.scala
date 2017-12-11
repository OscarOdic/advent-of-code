package com.oodic.aoc

import org.scalatest.{FlatSpec, Matchers}

class Day3Test extends FlatSpec with Matchers {
  it should "answer first part" in {
    Day3.resolveFirst(12) should be(3)
    Day3.resolveFirst(23) should be(2)
    Day3.resolveFirst(1024) should be(31)

    Day3.resolveFirst(Day3.input) should be(552)
  }

  it should "answer second part" in {
    Day3.resolveSecond(Day3.input) should be(330785)
  }
}
