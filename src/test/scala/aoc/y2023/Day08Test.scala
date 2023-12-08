package aoc.y2023

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day08Test extends AnyFlatSpec with Matchers {
  private val testInput1 = (
    "RL",
    List(
      "AAA = (BBB, CCC)",
      "BBB = (DDD, EEE)",
      "CCC = (ZZZ, GGG)",
      "DDD = (DDD, DDD)",
      "EEE = (EEE, EEE)",
      "GGG = (GGG, GGG)",
      "ZZZ = (ZZZ, ZZZ)"
    )
  )

  private val testInput2 = (
    "LLR",
    List(
      "AAA = (BBB, BBB)",
      "BBB = (AAA, ZZZ)",
      "ZZZ = (ZZZ, ZZZ)"
    )
  )

  private val testInput3 = (
    "LR",
    List(
      "11A = (11B, XXX)",
      "11B = (XXX, 11Z)",
      "11Z = (11B, XXX)",
      "22A = (22B, XXX)",
      "22B = (22C, 22C)",
      "22C = (22Z, 22Z)",
      "22Z = (22B, 22B)",
      "XXX = (XXX, XXX)"
    )
  )

  "Day08 - 2023" should "solve first part" in {
    Day08.part1(testInput1) should equal(2)
    Day08.part1(testInput2) should equal(6)

    Day08.part1(Day08.input) should equal(16697)
  }

  it should "solve second part" in {
    Day08.part2(testInput3) should equal(6)

    Day08.part2(Day08.input) should equal(10668805667831L)
  }
}
