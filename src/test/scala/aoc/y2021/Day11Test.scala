package aoc.y2021

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day11Test extends AnyFlatSpec with Matchers {
  val testInput =
    (for {
      (line, y) <- List(
        "5483143223",
        "2745854711",
        "5264556173",
        "6141336146",
        "6357385478",
        "4167524645",
        "2176841721",
        "6882881134",
        "4846848554",
        "5283751526"
      ).zipWithIndex
      (value, x) <- line.zipWithIndex
    } yield (x, y) -> value.asDigit)
      .toMap

  "Day11 - 2021" should "solve first part" in {
    Day11.part1(testInput) should equal(1656)

    Day11.part1(Day11.input) should equal(1705)
  }

  it should "solve second part" in {
    Day11.part2(testInput) should equal(195)

    Day11.part2(Day11.input) should equal(265)
  }
}
