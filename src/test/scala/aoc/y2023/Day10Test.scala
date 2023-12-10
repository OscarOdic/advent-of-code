package aoc.y2023

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day10Test extends AnyFlatSpec with Matchers {
  private val testInput1 = List(
    ".....",
    ".S-7.",
    ".|.|.",
    ".L-J.",
    "....."
  )

  private val testInput2 = List(
    "..F7.",
    ".FJ|.",
    "SJ.L7",
    "|F--J",
    "LJ..."
  )

  private val testInput3 = List(
    "...........",
    ".S-------7.",
    ".|F-----7|.",
    ".||.....||.",
    ".||.....||.",
    ".|L-7.F-J|.",
    ".|..|.|..|.",
    ".L--J.L--J.",
    "..........."
  )

  private val testInput4 = List(
    ".F----7F7F7F7F-7....",
    ".|F--7||||||||FJ....",
    ".||.FJ||||||||L7....",
    "FJL7L7LJLJ||LJ.L-7..",
    "L--J.L7...LJS7F-7L7.",
    "....F-J..F7FJ|L7L7L7",
    "....L7.F7||L7|.L7L7|",
    ".....|FJLJ|FJ|F7|.LJ",
    "....FJL-7.||.||||...",
    "....L---J.LJ.LJLJ..."
  )

  private val testInput5 = List(
    "FF7FSF7F7F7F7F7F---7",
    "L|LJ||||||||||||F--J",
    "FL-7LJLJ||||||LJL-77",
    "F--JF--7||LJLJ7F7FJ-",
    "L---JF-JLJ.||-FJLJJ7",
    "|F|F-JF---7F7-L7L|7|",
    "|FFJF7L7F-JF7|JL---7",
    "7-L-JL7||F7|L7F-7F7|",
    "L.L7LFJ|||||FJL7||LJ",
    "L7JLJL-JLJLJL--JLJ.L"
  )

  "Day10 - 2023" should "solve first part" in {
    Day10.part1(testInput1) should equal(4)
    Day10.part1(testInput2) should equal(8)
    Day10.part1(testInput3) should equal(23)
    Day10.part1(testInput4) should equal(70)
    Day10.part1(testInput5) should equal(80)

    Day10.part1(Day10.input) should equal(6867)
  }

  it should "solve second part" in {
    Day10.part2(testInput1) should equal(1)
    Day10.part2(testInput2) should equal(1)
    Day10.part2(testInput3) should equal(4)
    Day10.part2(testInput4) should equal(8)
    Day10.part2(testInput5) should equal(10)

    Day10.part2(Day10.input) should equal(595)
  }
}
