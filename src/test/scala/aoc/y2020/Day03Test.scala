package aoc.y2020

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day03Test extends AnyFlatSpec with Matchers {
  val testInput = Vector(
    "..##.......",
    "#...#...#..",
    ".#....#..#.",
    "..#.#...#.#",
    ".#...##..#.",
    "..#.##.....",
    ".#.#.#....#",
    ".#........#",
    "#.##...#...",
    "#...##....#",
    ".#..#...#.#"
  ).map(_.toVector)

  "Day03 - 2020" should "solve first part" in {
    Day03.part1(testInput) should equal(7)

    Day03.part1(Day03.input) should equal(254)
  }

  it should "solve second part" in {
    Day03.part2(testInput) should equal(336)

    Day03.part2(Day03.input) should equal(1666768320)
  }
}
