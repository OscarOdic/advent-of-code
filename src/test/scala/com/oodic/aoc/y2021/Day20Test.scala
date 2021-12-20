package com.oodic.aoc.y2021

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day20Test extends AnyFlatSpec with Matchers {
  val testInput = (
    "..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#",

    (for {
      (line, y) <- List("#..#.",
        "#....",
        "##..#",
        "..#..",
        "..###").zipWithIndex
      (value, x) <- line.zipWithIndex
    } yield (x, y) -> value).toMap
  )

  "Day20 - 2021" should "solve first part" in {
    Day20.part1(testInput) should equal(35)

    Day20.part1(Day20.input) should equal(5846)
  }

  it should "solve second part" in {
    Day20.part2(testInput) should equal(3351)

    Day20.part2(Day20.input) should equal(21149)
  }
}
