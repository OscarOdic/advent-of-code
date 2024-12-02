package aoc.y2024

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day01Test extends AnyFlatSpec with Matchers {
  private val testInput = List(
    (3, 4),
    (4, 3),
    (2, 5),
    (1, 3),
    (3, 9),
    (3, 3)
  )

  "Day01 - 2024" should "solve first part" in {
    Day01.part1(testInput) should equal(11)

    Day01.part1(Day01.input) should equal(2430334)
  }

  it should "solve second part" in {
    Day01.part2(testInput) should equal(31)

    Day01.part2(Day01.input) should equal(28786472)
  }
}
