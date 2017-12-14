package com.oodic.aoc

import org.scalatest.{FlatSpec, Matchers}

class Day06Test extends FlatSpec with Matchers {
  it should "answer first part" in {
    Day06.resolve(Vector(0,2,7,0))._1 should be(5)

    Day06.resolve(Day06.input)._1 should be(3156)
  }

  it should "answer second part" in {
    Day06.resolve(Vector(0,2,7,0))._2 should be(4)

    Day06.resolve(Day06.input)._2 should be(1610)
  }
}
