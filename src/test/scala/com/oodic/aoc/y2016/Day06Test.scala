package com.oodic.aoc.y2016

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day06Test extends AnyFlatSpec with Matchers {
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

  "Day06 - 2016" should "solve first part" in {
    Day06.part1(testInput) should equal("easter")

    Day06.part1(Day06.input) should equal("kjxfwkdh")
  }

  it should "solve second part" in {
    Day06.part2(testInput) should equal("advent")

    Day06.part2(Day06.input) should equal("xrwcsnps")
  }
}
