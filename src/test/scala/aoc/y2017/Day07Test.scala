package aoc.y2017

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day07Test extends AnyFlatSpec with Matchers {
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

  "Day07 - 2017" should "solve first part" in {
    Day07.part1(testInput) should equal("tknk")

    Day07.part1(Day07.input) should equal("qibuqqg")
  }

  it should "solve second part" in {
    Day07.part2(testInput) should equal(60)

    Day07.part2(Day07.input) should equal(1079)
  }
}
