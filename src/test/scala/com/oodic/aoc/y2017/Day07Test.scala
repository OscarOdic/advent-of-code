package com.oodic.aoc.y2017

import org.scalatest.{FlatSpec, Matchers}

class Day07Test extends FlatSpec with Matchers {
  val testInput = List(
    "pbga (66)",
    "xhth (57)",
    "ebii (61)",
    "havc (66)",
    "ktlj (57)",
    "fwft (72) -> ktlj, cntj, xhth",
    "qoyq (66)",
    "padx (45) -> pbga, havc, qoyq",
    "tknk (41) -> ugml, padx, fwft",
    "jptl (61)",
    "ugml (68) -> gyxo, ebii, jptl",
    "gyxo (61)",
    "cntj (57)"
  )

  "Day07 - 2017" should "answer first part" in {
    Day07.resolveFirst(testInput) should be("tknk")

    Day07.resolveFirst(Day07.input) should be("qibuqqg")
  }

  it should "answer second part" in {
    Day07.resolveSecond(testInput) should be(60)

    Day07.resolveSecond(Day07.input) should be(1079)
  }
}
