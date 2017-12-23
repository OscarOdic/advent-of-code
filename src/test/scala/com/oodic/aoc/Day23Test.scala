package com.oodic.aoc

import org.scalatest.{FlatSpec, Matchers}

class Day23Test extends FlatSpec with Matchers {
  it should "answer first part" in {
    Day23.resolveFirst(Day23.input) should be(5929)
  }

  it should "answer second part" in {
    Day23.resolveSecond(Day23.input) should be(907)
  }
}