package aoc.y2023

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day11Test extends AnyFlatSpec with Matchers {
  private val testInput = List(
    "...#......",
    ".......#..",
    "#.........",
    "..........",
    "......#...",
    ".#........",
    ".........#",
    "..........",
    ".......#..",
    "#...#....."
  )

  "Day11 - 2023" should "solve first part" in {
    Day11.part1(testInput) should equal(374)

    Day11.part1(Day11.input) should equal(9684228)
  }

  it should "solve second part" in {
    Day11.part2(testInput) should equal(82000210)

    Day11.part2(Day11.input) should equal(483844716556L)
  }
}
