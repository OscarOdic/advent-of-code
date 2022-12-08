package aoc.y2017

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day11Test extends AnyFlatSpec with Matchers {
  "Day11 - 2017" should "solve first part" in {
    Day11.part1(List("ne","ne","ne")) should equal(3)
    Day11.part1(List("ne","ne","sw","sw")) should equal(0)
    Day11.part1(List("ne","ne","s","s")) should equal(2)
    Day11.part1(List("se","sw","se","sw","sw")) should equal(3)

    Day11.part1(Day11.input) should equal(759)
  }

  it should "solve second part" in {
    Day11.part2(Day11.input) should equal(1501)
  }
}
