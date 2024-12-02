package aoc.y2024

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day02Test extends AnyFlatSpec with Matchers {
  private val testInput = List(
    List(7, 6, 4, 2, 1),
    List(1, 2, 7, 8, 9),
    List(9, 7, 6, 2, 1),
    List(1, 3, 2, 4, 5),
    List(8, 6, 4, 4, 1),
    List(1, 3, 6, 7, 9)
  )

  "Day02 - 2024" should "solve first part" in {
    Day02.part1(testInput) should equal(2)

    Day02.part1(Day02.input) should equal(379)
  }

  it should "solve second part" in {
    Day02.part2(testInput) should equal(4)

    Day02.part2(Day02.input) should equal(430)
  }
}
