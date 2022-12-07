package com.oodic.aoc.y2022

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day07Test extends AnyFlatSpec with Matchers {
  private val testInput = List(
    "$ cd /",
    "$ ls",
    "dir a",
    "14848514 b.txt",
    "8504156 c.dat",
    "dir d",
    "$ cd a",
    "$ ls",
    "dir e",
    "29116 f",
    "2557 g",
    "62596 h.lst",
    "$ cd e",
    "$ ls",
    "584 i",
    "$ cd ..",
    "$ cd ..",
    "$ cd d",
    "$ ls",
    "4060174 j",
    "8033020 d.log",
    "5626152 d.ext",
    "7214296 k"
  )

  "Day07 - 2022" should "solve first part" in {
    Day07.part1(testInput) should equal(95437)

    Day07.part1(Day07.input) should equal(1778099)
  }

  it should "solve second part" in {
    Day07.part2(testInput) should equal(24933642)

    Day07.part2(Day07.input) should equal(1623571)
  }
}
