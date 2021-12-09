package com.oodic.aoc.y2020

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day16Test extends AnyFlatSpec with Matchers {
  val testInput = (
    List("class: 1-3 or 5-7",
      "row: 6-11 or 33-44",
      "seat: 13-40 or 45-50"),

    List("your ticket:",
      "7,1,14"),

    List("nearby tickets:",
      "7,3,47",
      "40,4,50",
      "55,2,20",
      "38,6,12")
  )

  "Day16 - 2020" should "solve first part" in {
    Day16.part1(testInput) should equal(71)

    Day16.part1(Day16.input) should equal(25972)
  }

  it should "solve second part" in {
    Day16.part2(Day16.input) should equal(622670335901L)
  }
}
