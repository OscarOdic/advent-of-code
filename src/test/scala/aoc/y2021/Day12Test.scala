package aoc.y2021

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day12Test extends AnyFlatSpec with Matchers {
  val testInput1 = List(
    "start-A",
    "start-b",
    "A-c",
    "A-b",
    "b-d",
    "A-end",
    "b-end"
  )

  val testInput2 = List(
    "dc-end",
    "HN-start",
    "start-kj",
    "dc-start",
    "dc-HN",
    "LN-dc",
    "HN-end",
    "kj-sa",
    "kj-HN",
    "kj-dc"
  )

  val testInput3 = List(
    "fs-end",
    "he-DX",
    "fs-he",
    "start-DX",
    "pj-DX",
    "end-zg",
    "zg-sl",
    "zg-pj",
    "pj-he",
    "RW-he",
    "fs-DX",
    "pj-RW",
    "zg-RW",
    "start-pj",
    "he-WI",
    "zg-he",
    "pj-fs",
    "start-RW"
  )

  "Day12 - 2021" should "solve first part" in {
    Day12.part1(testInput1) should equal(10)
    Day12.part1(testInput2) should equal(19)
    Day12.part1(testInput3) should equal(226)

    Day12.part1(Day12.input) should equal(4167)
  }

  it should "solve second part" in {
    Day12.part2(testInput1) should equal(36)
    Day12.part2(testInput2) should equal(103)
    Day12.part2(testInput3) should equal(3509)

    Day12.part2(Day12.input) should equal(98441)
  }
}