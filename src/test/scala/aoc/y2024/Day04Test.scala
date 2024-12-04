package aoc.y2024

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day04Test extends AnyFlatSpec with Matchers {
  private val testInput = List(
    "MMMSXXMASM".toList,
    "MSAMXMSMSA".toList,
    "AMXSXMAAMM".toList,
    "MSAMASMSMX".toList,
    "XMASAMXAMM".toList,
    "XXAMMXXAMA".toList,
    "SMSMSASXSS".toList,
    "SAXAMASAAA".toList,
    "MAMMMXMMMM".toList,
    "MXMXAXMASX".toList,
  )

  "Day04 - 2024" should "solve first part" in {
    Day04.part1(testInput) should equal(18)

    Day04.part1(Day04.input) should equal(2378)
  }

  it should "solve second part" in {
    Day04.part2(testInput) should equal(9)

    Day04.part2(Day04.input) should equal(1796)
  }
}
