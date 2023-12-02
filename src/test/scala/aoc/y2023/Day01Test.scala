package aoc.y2023

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day01Test extends AnyFlatSpec with Matchers {
  private val testInput = List(
    "1abc2",
    "pqr3stu8vwx",
    "a1b2c3d4e5f",
    "treb7uchet"
  )

  private val testInput2 = List(
    "two1nine",
    "eightwothree",
    "abcone2threexyz",
    "xtwone3four",
    "4nineeightseven2",
    "zoneight234",
    "7pqrstsixteen"
  )

  "Day01 - 2023" should "solve first part" in {
    Day01.part1(testInput) should equal(142)

    Day01.part1(Day01.input) should equal(53974)
  }

  it should "solve second part" in {
    Day01.part2(testInput2) should equal(281)

    Day01.part2(Day01.input) should equal(52840)
  }
}
