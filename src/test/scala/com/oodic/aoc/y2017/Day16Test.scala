package com.oodic.aoc.y2017

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day16Test extends AnyFlatSpec with Matchers {
  val testInput = List(
    "s1",
    "x3/4",
    "pe/b"
  )

  "Day16 - 2017" should "solve first part" in {
    Day16.resolveWithPrograms(testInput, "abcde") should equal("baedc")

    Day16.part1(Day16.input) should equal("jcobhadfnmpkglie")
  }

  it should "solve second part" in {
    Day16.part2(Day16.input) should equal("pclhmengojfdkaib")
  }
}