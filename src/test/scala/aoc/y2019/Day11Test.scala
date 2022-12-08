package aoc.y2019

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day11Test extends AnyFlatSpec with Matchers {
  "Day11 - 2019" should "solve first part" in {
    Day11.part1(Day11.input) should equal(1732)
  }

  it should "solve second part" in {
    val testInput = List(
      "..##..###...##..#....####.#..#.#..#...##...",
      ".#..#.#..#.#..#.#....#....#..#.#..#....#...",
      ".#..#.###..#....#....###..#..#.####....#...",
      ".####.#..#.#....#....#....#..#.#..#....#...",
      ".#..#.#..#.#..#.#....#....#..#.#..#.#..#...",
      ".#..#.###...##..####.#.....##..#..#..##...."
    )

    Day11.part2(Day11.input) should equal("\n" + testInput.mkString("\n"))
  }
}
