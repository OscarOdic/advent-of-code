package com.oodic.aoc.y2019

import org.scalatest.{FlatSpec, Matchers}

class Day11Test extends FlatSpec with Matchers {
  "Day11 - 2019" should "answer first part" in {
    Day11.resolveFirst(Day11.input) should be(1732)
  }

  it should "answer second part" in {
    val testInput = List(
      "..##..###...##..#....####.#..#.#..#...##...",
      ".#..#.#..#.#..#.#....#....#..#.#..#....#...",
      ".#..#.###..#....#....###..#..#.####....#...",
      ".####.#..#.#....#....#....#..#.#..#....#...",
      ".#..#.#..#.#..#.#....#....#..#.#..#.#..#...",
      ".#..#.###...##..####.#.....##..#..#..##...."
    )

    Day11.resolveSecond(Day11.input) should be("\n" + testInput.mkString("\n"))
  }
}
