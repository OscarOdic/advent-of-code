package aoc.y2022

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day09Test extends AnyFlatSpec with Matchers {
  private val testInput1 = List(
    "R 4",
    "U 4",
    "L 3",
    "D 1",
    "R 4",
    "D 1",
    "L 5",
    "R 2",
  )

  private val testInput2 = List(
    "R 5",
    "U 8",
    "L 8",
    "D 3",
    "R 17",
    "D 10",
    "L 25",
    "U 20"
  )

  "Day09 - 2022" should "solve first part" in {
    Day09.part1(testInput1) should equal(13)

    Day09.part1(Day09.input) should equal(6081)
  }

  it should "solve second part" in {
    Day09.part2(testInput1) should equal(1)
    Day09.part2(testInput2) should equal(36)

    Day09.part2(Day09.input) should equal(2487)
  }
}
