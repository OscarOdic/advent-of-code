package com.oodic.aoc

import org.scalatest.{FlatSpec, Matchers}

class Day22Test extends FlatSpec with Matchers {
  val testInput: Map[(Int, Int), Boolean] = Map(
    (0,0) -> false, (1,0) -> false, (2,0) -> true,
    (0,1) -> true, (1,1) -> false, (2,1) -> false,
    (0,2) -> false, (1,2) -> false, (2,2) -> false
  )

  it should "answer first part" in {
    Day22.resolveFirst(testInput) should be(5587)

    Day22.resolveFirst(Day22.input) should be(5256)
  }

  it should "answer second part" in {
    Day22.resolveSecond(testInput) should be(2511944)

    Day22.resolveSecond(Day22.input) should be(2511345)
  }
}