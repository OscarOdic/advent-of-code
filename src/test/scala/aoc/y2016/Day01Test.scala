package aoc.y2016

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day01Test extends AnyFlatSpec with Matchers {
  "Day01 - 2016" should "solve first part" in {
    Day01.part1(List("R2", "L3")) should equal(5)
    Day01.part1(List("R2", "R2", "R2")) should equal(2)
    Day01.part1(List("R5", "L5", "R5", "R3")) should equal(12)

    Day01.part1(Day01.input) should equal(241)
  }

  it should "solve second part" in {
    Day01.part2(List("R8", "R4", "R4", "R8")) should equal(4)

    Day01.part2(Day01.input) should equal(116)
  }
}
