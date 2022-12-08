package aoc.y2021

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day05Test extends AnyFlatSpec with Matchers {
  val testInput = List(
    "0,9 -> 5,9",
    "8,0 -> 0,8",
    "9,4 -> 3,4",
    "2,2 -> 2,1",
    "7,0 -> 7,4",
    "6,4 -> 2,0",
    "0,9 -> 2,9",
    "3,4 -> 1,4",
    "0,0 -> 8,8",
    "5,5 -> 8,2"
  )

  "Day05 - 2021" should "solve first part" in {
    Day05.part1(testInput) should equal(5)

    Day05.part1(Day05.input) should equal(7468)
  }

  it should "solve second part" in {
    Day05.part2(testInput) should equal(12)

    Day05.part2(Day05.input) should equal(22364)
  }
}
