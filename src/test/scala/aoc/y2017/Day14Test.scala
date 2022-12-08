package aoc.y2017

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day14Test extends AnyFlatSpec with Matchers {
  val testInput = Day14.getGrid("flqrgnkx")

  "Day14 - 2017" should "solve first part" in {
    Day14.part1(testInput) should equal(8108)

    Day14.part1(Day14.input) should equal(8226)
  }

  it should "solve second part" in {
    Day14.part2(testInput) should equal(1242)

    Day14.part2(Day14.input) should equal(1128)
  }
}
