package aoc.y2019

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day10Test extends AnyFlatSpec with Matchers {
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

  "Day10 - 2019" should "solve first part" in {
    Day10.part1(testInput1) should equal(33)
    Day10.part1(testInput2) should equal(35)
    Day10.part1(testInput3) should equal(41)
    Day10.part1(testInput4) should equal(210)

    Day10.part1(Day10.input) should equal(292)
  }

  it should "solve second part" in {
    Day10.part2(testInput4) should equal(802)

    Day10.part2(Day10.input) should equal(317)
  }
}
