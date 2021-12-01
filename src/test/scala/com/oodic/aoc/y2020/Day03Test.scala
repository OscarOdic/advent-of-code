package com.oodic.aoc.y2020

import org.scalatest.{FlatSpec, Matchers}

class Day03Test extends FlatSpec with Matchers {
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

  "Day03 - 2020" should "answer first part" in {
    Day03.resolveFirst(testInput) should be(7)

    Day03.resolveFirst(Day03.input) should be(254)
  }

  it should "answer second part" in {
    Day03.resolveSecond(testInput) should be(336)

    Day03.resolveSecond(Day03.input) should be(1666768320)
  }
}
