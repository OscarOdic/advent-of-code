package com.oodic.aoc.y2016

import org.scalatest.{FlatSpec, Matchers}

class Day08Test extends FlatSpec with Matchers {
  val testInput: List[String] = List(
    "rect 3x2",
    "rotate column x=1 by 1",
    "rotate row y=0 by 4",
    "rotate column x=1 by 1"
  )

  "Day08 - 2016" should "answer first part" in {

    Day08.resolveFirst(testInput) should be(6)

    Day08.resolveFirst(Day08.input) should be(115)
  }

  it should "answer second part" in {
    val result = List("",
      "####.####.####.#...##..#.####.###..####..###...##.",
      "#....#....#....#...##.#..#....#..#.#......#.....#.",
      "###..###..###...#.#.##...###..#..#.###....#.....#.",
      "#....#....#......#..#.#..#....###..#......#.....#.",
      "#....#....#......#..#.#..#....#.#..#......#..#..#.",
      "####.#....####...#..#..#.#....#..#.#.....###..##.."
    ).mkString("\n")

    Day08.resolveSecond(Day08.input) should be(result)
  }
}
