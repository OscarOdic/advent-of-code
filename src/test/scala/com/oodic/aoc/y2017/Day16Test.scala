package com.oodic.aoc.y2017

import org.scalatest.{FlatSpec, Matchers}

class Day16Test extends FlatSpec with Matchers {
  val testInput = List(
    "s1",
    "x3/4",
    "pe/b"
  )

  "Day16 - 2017" should "answer first part" in {
    Day16.resolveWithPrograms(testInput, "abcde") should be("baedc")

    Day16.resolveFirst(Day16.input) should be("jcobhadfnmpkglie")
  }

  it should "answer second part" in {
    Day16.resolveSecond(Day16.input) should be("pclhmengojfdkaib")
  }
}