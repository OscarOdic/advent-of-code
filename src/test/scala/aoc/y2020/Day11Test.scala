package aoc.y2020

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day11Test extends AnyFlatSpec with Matchers {
  val testInput1 = (for {
    (value, y) <- List(
      "L.LL.LL.LL",
      "LLLLLLL.LL",
      "L.L.L..L..",
      "LLLL.LL.LL",
      "L.LL.LL.LL",
      "L.LLLLL.LL",
      "..L.L.....",
      "LLLLLLLLLL",
      "L.LLLLLL.L",
      "L.LLLLL.LL"
    ).zipWithIndex
    (char, x) <- value.zipWithIndex
  } yield ((x, y), char)
    ).toMap


  "Day11 - 2020" should "solve first part" in {
    Day11.part1(testInput1) should equal(37)

    Day11.part1(Day11.input) should equal(2251)
  }

  it should "solve second part" in {
    Day11.part2(testInput1) should equal(26)

    Day11.part2(Day11.input) should equal(2019)
  }
}
