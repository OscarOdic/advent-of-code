package com.oodic.aoc.y2020

import org.scalatest.{FlatSpec, Matchers}

class Day16Test extends FlatSpec with Matchers {
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

  "Day16 - 2020" should "answer first part" in {
    Day16.resolveFirst(testInput) should be(71)

    Day16.resolveFirst(Day16.input) should be(25972)
  }

  it should "answer second part" in {
    Day16.resolveSecond(Day16.input) should be(622670335901L)
  }
}
