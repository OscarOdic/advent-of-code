package com.oodic.aoc

import org.scalatest.{FlatSpec, Matchers}

class Day17Test extends FlatSpec with Matchers {
  it should "answer first part" in {
    Day17.resolveFirst(3) should be(638)

    Day17.resolveFirst(Day17.input) should be(1244)
  }

  it should "answer second part" in {
    Day17.resolveSecond(Day17.input) should be(11162912)
  }
}