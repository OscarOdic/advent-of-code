package com.oodic.aoc.y2017

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day22Test extends AnyFlatSpec with Matchers {
  val testInput: Map[(Int, Int), Boolean] = Map(
    (0,0) -> false, (1,0) -> false, (2,0) -> true,
    (0,1) -> true, (1,1) -> false, (2,1) -> false,
    (0,2) -> false, (1,2) -> false, (2,2) -> false
  )

  "Day22 - 2017" should "solve first part" in {
    Day22.part1(testInput) should equal(5587)

    Day22.part1(Day22.input) should equal(5256)
  }

  it should "solve second part" in {
    Day22.part2(testInput) should equal(2511944)

    Day22.part2(Day22.input) should equal(2511345)
  }
}