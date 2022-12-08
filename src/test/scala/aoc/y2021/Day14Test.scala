package aoc.y2021

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day14Test extends AnyFlatSpec with Matchers {
  val testInput = (
    "NNCB",
    List("CH -> B",
      "HH -> N",
      "CB -> H",
      "NH -> C",
      "HB -> C",
      "HC -> B",
      "HN -> C",
      "NN -> C",
      "BH -> H",
      "NC -> B",
      "NB -> B",
      "BN -> B",
      "BB -> N",
      "BC -> B",
      "CC -> N",
      "CN -> C"
    )
  )

  "Day14 - 2021" should "solve first part" in {
    Day14.part1(testInput) should equal(1588)

    Day14.part1(Day14.input) should equal(2587)
  }

  it should "solve second part" in {
    Day14.part2(testInput) should equal(2188189693529L)

    Day14.part2(Day14.input) should equal(3318837563123L)
  }
}