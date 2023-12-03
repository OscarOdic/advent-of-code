package aoc.y2023

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day03Test extends AnyFlatSpec with Matchers {
  private val testInput = List(
    "467..114..",
    "...*......",
    "..35..633.",
    "......#...",
    "617*......",
    ".....+.58.",
    "..592.....",
    "......755.",
    "...$.*....",
    ".664.598.."
  )

  "Day03 - 2023" should "solve first part" in {
    Day03.part1(testInput) should equal(4361)

    Day03.part1(Day03.input) should equal(525911)
  }

  it should "solve second part" in {
    Day03.part2(testInput) should equal(467835)

    Day03.part2(Day03.input) should equal(75805607)
  }
}
