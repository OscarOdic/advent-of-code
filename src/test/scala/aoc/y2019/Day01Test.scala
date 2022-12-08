package aoc.y2019

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day01Test extends AnyFlatSpec with Matchers {
  "Day01 - 2019" should "solve first part" in {
    Day01.part1(List(12)) should equal(2)
    Day01.part1(List(14)) should equal(2)
    Day01.part1(List(1969)) should equal(654)
    Day01.part1(List(100756)) should equal(33583)
    Day01.part1(List(12, 14, 1969, 100756)) should equal(34241)

    Day01.part1(Day01.input) should equal(3302760)
  }

  it should "solve second part" in {
    Day01.part2(List(12)) should equal(2)
    Day01.part2(List(14)) should equal(2)
    Day01.part2(List(1969)) should equal(966)
    Day01.part2(List(100756)) should equal(50346)
    Day01.part2(List(12, 14, 1969, 100756)) should equal(51316)

    Day01.part2(Day01.input) should equal(4951265)
  }
}
