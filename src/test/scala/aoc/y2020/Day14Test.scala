package aoc.y2020

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day14Test extends AnyFlatSpec with Matchers {
  "Day14 - 2020" should "solve first part" in {
    val testInput = List(
      "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X",
      "mem[8] = 11",
      "mem[7] = 101",
      "mem[8] = 0"
    )

    Day14.part1(testInput) should equal(165L)

    Day14.part1(Day14.input) should equal(12512013221615L)
  }

  it should "solve second part" in {
    val testInput = List(
      "mask = 000000000000000000000000000000X1001X",
      "mem[42] = 100",
      "mask = 00000000000000000000000000000000X0XX",
      "mem[26] = 1"
    )

    Day14.part2(testInput) should equal(208L)

    Day14.part2(Day14.input) should equal(3905642473893L)
  }
}
