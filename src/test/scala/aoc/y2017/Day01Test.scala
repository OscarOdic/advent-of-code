package aoc.y2017

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day01Test extends AnyFlatSpec with Matchers {
  "Day01 - 2017" should "solve first part" in {
    Day01.part1(List(1,1,2,2)) should equal(3)
    Day01.part1(List(1,1,1,1)) should equal(4)
    Day01.part1(List(1,2,3,4)) should equal(0)
    Day01.part1(List(9,1,2,1,2,1,2,9)) should equal(9)

    Day01.part1(Day01.input) should equal(1203)
  }

  it should "solve second part" in {
    Day01.part2(List(1,2,1,2)) should equal(6)
    Day01.part2(List(1,2,2,1)) should equal(0)
    Day01.part2(List(1,2,3,4,2,5)) should equal(4)
    Day01.part2(List(1,2,3,1,2,3)) should equal(12)
    Day01.part2(List(1,2,1,3,1,4,1,5)) should equal(4)

    Day01.part2(Day01.input) should equal(1146)
  }
}
