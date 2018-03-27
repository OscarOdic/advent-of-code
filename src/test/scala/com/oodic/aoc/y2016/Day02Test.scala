package com.oodic.aoc.y2016

import org.scalatest.{FlatSpec, Matchers}

class Day02Test extends FlatSpec with Matchers {
  val testInput: List[String] = List(
    "ULL",
    "RRDDD",
    "LURDL",
    "UUUUD"
  )

  "Day02 - 2016" should "answer first part" in {
    Day02.resolveFirst(testInput) should be("1985")

    Day02.resolveFirst(Day02.input) should be("98575")
  }

  it should "answer second part" in {
    Day02.resolveSecond(testInput) should be("5DB3")

    Day02.resolveSecond(Day02.input) should be("CD8D4")
  }
}
