package aoc.y2023

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day09Test extends AnyFlatSpec with Matchers {
  private val testInput = List(
    List(0, 3, 6, 9, 12, 15),
    List(1, 3, 6, 10, 15, 21),
    List(10, 13, 16, 21, 30, 45)
  )

  "Day09 - 2023" should "solve first part" in {
    Day09.part1(testInput) should equal(114)

    Day09.part1(Day09.input) should equal(2008960228)
  }

  it should "solve second part" in {
    Day09.part2(testInput) should equal(2)

    Day09.part2(Day09.input) should equal(1097)
  }
}
