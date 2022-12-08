package aoc.y2021

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day15Test extends AnyFlatSpec with Matchers {
  val testInput =
    (for {
      (line, y) <- List(
        "1163751742",
        "1381373672",
        "2136511328",
        "3694931569",
        "7463417111",
        "1319128137",
        "1359912421",
        "3125421639",
        "1293138521",
        "2311944581"
      ).zipWithIndex
      (value, x) <- line.zipWithIndex
    } yield (x, y) -> value.asDigit)
      .toMap

  "Day15 - 2021" should "solve first part" in {
    Day15.part1(testInput) should equal(40)

    Day15.part1(Day15.input) should equal(429)
  }

  it should "solve second part" in {
    Day15.part2(testInput) should equal(315)

    Day15.part2(Day15.input) should equal(2844)
  }
}
