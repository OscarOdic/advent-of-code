package com.oodic.aoc.y2016

import org.scalatest.{FlatSpec, Matchers}

class Day21Test extends FlatSpec with Matchers {
  "Day21 - 2016" should "answer first part" in {
    Day21.resolveFirst(Day21.input) should be("gcedfahb")
  }

  it should "answer second part" in {
    Day21.resolveSecond(Day21.input) should be("hegbdcfa")
  }
}
