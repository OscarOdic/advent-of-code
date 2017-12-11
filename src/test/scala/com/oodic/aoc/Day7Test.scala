package com.oodic.aoc

import org.scalatest.{FlatSpec, Matchers}

class Day7Test extends FlatSpec with Matchers {
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

  it should "answer first part" in {
    Day7.resolveFirst(testInput) should be("tknk")

    Day7.resolveFirst(Day7.input) should be("qibuqqg")
  }

  it should "answer second part" in {
    Day7.resolveSecond(testInput) should be(60)

    Day7.resolveSecond(Day7.input) should be(1079)
  }
}
