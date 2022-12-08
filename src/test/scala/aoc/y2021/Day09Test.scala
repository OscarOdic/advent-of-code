package aoc.y2021

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day09Test extends AnyFlatSpec with Matchers {
  val testInput =
    (for {
      (line, y) <- List(
        List(2, 1, 9, 9, 9, 4, 3, 2, 1, 0),
        List(3, 9, 8, 7, 8, 9, 4, 9, 2, 1),
        List(9, 8, 5, 6, 7, 8, 9, 8, 9, 2),
        List(8, 7, 6, 7, 8, 9, 6, 7, 8, 9),
        List(9, 8, 9, 9, 9, 6, 5, 6, 7, 8)
      ).zipWithIndex
      (value, x) <- line.zipWithIndex
    } yield (x, y) -> value)
      .toMap

  "Day09 - 2021" should "solve first part" in {
    Day09.part1(testInput) should equal(15)

    Day09.part1(Day09.input) should equal(562)
  }

  it should "solve second part" in {
    Day09.part2(testInput) should equal(1134)

    Day09.part2(Day09.input) should equal(1076922)
  }
}
