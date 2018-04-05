package com.oodic.aoc.y2016

import org.scalatest.{FlatSpec, Matchers}

class Day06Test extends FlatSpec with Matchers {
  val testInput: List[String] = List(
    "eedadn",
    "drvtee",
    "eandsr",
    "raavrd",
    "atevrs",
    "tsrnev",
    "sdttsa",
    "rasrtv",
    "nssdts",
    "ntnada",
    "svetve",
    "tesnvt",
    "vntsnd",
    "vrdear",
    "dvrsen",
    "enarar"
  ).map(_.toList).transpose.map(_.mkString)

  "Day06 - 2016" should "answer first part" in {
    Day06.resolveFirst(testInput) should be("easter")

    Day06.resolveFirst(Day06.input) should be("kjxfwkdh")
  }

  it should "answer second part" in {
    Day06.resolveSecond(testInput) should be("advent")

    Day06.resolveSecond(Day06.input) should be("xrwcsnps")
  }
}
