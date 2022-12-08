package aoc.y2021

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day13Test extends AnyFlatSpec with Matchers {
  val testInput = (
    List((6, 10),
      (0, 14),
      (9, 10),
      (0, 3),
      (10, 4),
      (4, 11),
      (6, 0),
      (6, 12),
      (4, 1),
      (0, 13),
      (10, 12),
      (3, 4),
      (3, 0),
      (8, 4),
      (1, 10),
      (2, 14),
      (8, 10),
      (9, 0)),

    List("fold along y=7",
      "fold along x=5")
  )

  "Day13 - 2021" should "solve first part" in {
    Day13.part1(testInput) should equal(17)

    Day13.part1(Day13.input) should equal(693)
  }

  it should "solve second part" in {
    val resultTestInput = List(
      "#####",
      "#...#",
      "#...#",
      "#...#",
      "#####"
    ).mkString("\n")

    val resultInput = List(
      "#..#..##..#....####.###...##..####.#..#",
      "#..#.#..#.#.......#.#..#.#..#....#.#..#",
      "#..#.#....#......#..#..#.#..#...#..#..#",
      "#..#.#....#.....#...###..####..#...#..#",
      "#..#.#..#.#....#....#.#..#..#.#....#..#",
      ".##...##..####.####.#..#.#..#.####..##."
    ).mkString("\n")

    Day13.part2(testInput) should equal(resultTestInput)

    Day13.part2(Day13.input) should equal(resultInput)
  }
}