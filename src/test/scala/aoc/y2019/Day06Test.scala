package aoc.y2019

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day06Test extends AnyFlatSpec with Matchers {
  val testInput = Vector(
    "COM)B",
    "B)C",
    "C)D",
    "D)E",
    "E)F",
    "B)G",
    "G)H",
    "D)I",
    "E)J",
    "J)K",
    "K)L"
  )

  "Day06 - 2019" should "solve first part" in {
    Day06.part1(testInput) should be (42)

    Day06.part1(Day06.input) should equal(119831)
  }

  it should "solve second part" in {
    Day06.part2(testInput ++ Vector("K)YOU","I)SAN")) should be (4)

    Day06.part2(Day06.input) should equal(322)
  }
}
