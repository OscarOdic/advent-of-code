package com.oodic.aoc.y2019

import org.scalatest.{FlatSpec, Matchers}

class Day10Test extends FlatSpec with Matchers {
  val testInput1 = Vector(
    "......#.#.",
    "#..#.#....",
    "..#######.",
    ".#.#.###..",
    ".#..#.....",
    "..#....#.#",
    "#..#....#.",
    ".##.#..###",
    "##...#..#.",
    ".#....####"
  ).map(_.toVector.map(_.toString))

  val testInput2 = Vector(
    "#.#...#.#.",
    ".###....#.",
    ".#....#...",
    "##.#.#.#.#",
    "....#.#.#.",
    ".##..###.#",
    "..#...##..",
    "..##....##",
    "......#...",
    ".####.###."
  ).map(_.toVector.map(_.toString))

  val testInput3 = Vector(
    ".#..#..###",
    "####.###.#",
    "....###.#.",
    "..###.##.#",
    "##.##.#.#.",
    "....###..#",
    "..#.#..#.#",
    "#..#.#.###",
    ".##...##.#",
    ".....#.#.."
  ).map(_.toVector.map(_.toString))

  val testInput4 = Vector(
    ".#..##.###...#######",
    "##.############..##.",
    ".#.######.########.#",
    ".###.#######.####.#.",
    "#####.##.#.##.###.##",
    "..#####..#.#########",
    "####################",
    "#.####....###.#.#.##",
    "##.#################",
    "#####.##.###..####..",
    "..######..##.#######",
    "####.##.####...##..#",
    ".#####..#.######.###",
    "##...#.##########...",
    "#.##########.#######",
    ".####.#.###.###.#.##",
    "....##.##.###..#####",
    ".#.#.###########.###",
    "#.#.#.#####.####.###",
    "###.##.####.##.#..##"
  ).map(_.toVector.map(_.toString))

  "Day10 - 2019" should "answer first part" in {
    Day10.resolveFirst(testInput1) should be(33)
    Day10.resolveFirst(testInput2) should be(35)
    Day10.resolveFirst(testInput3) should be(41)
    Day10.resolveFirst(testInput4) should be(210)

    Day10.resolveFirst(Day10.input) should be(292)
  }

  it should "answer second part" in {
    Day10.resolveSecond(testInput4) should be(802)

    Day10.resolveSecond(Day10.input) should be(317)
  }
}
