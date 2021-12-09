package com.oodic.aoc.y2016

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day08Test extends AnyFlatSpec with Matchers {
  val testInput: List[String] = List(
    "rect 3x2",
    "rotate column x=1 by 1",
    "rotate row y=0 by 4",
    "rotate column x=1 by 1"
  )

  "Day08 - 2016" should "solve first part" in {

    Day08.part1(testInput) should equal(6)

    Day08.part1(Day08.input) should equal(115)
  }

  it should "solve second part" in {
    val result = List("",
      "####.####.####.#...##..#.####.###..####..###...##.",
      "#....#....#....#...##.#..#....#..#.#......#.....#.",
      "###..###..###...#.#.##...###..#..#.###....#.....#.",
      "#....#....#......#..#.#..#....###..#......#.....#.",
      "#....#....#......#..#.#..#....#.#..#......#..#..#.",
      "####.#....####...#..#..#.#....#..#.#.....###..##.."
    ).mkString("\n")

    Day08.part2(Day08.input) should equal(result)
  }
}
