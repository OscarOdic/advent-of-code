package aoc.y2017

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day23Test extends AnyFlatSpec with Matchers {
  "Day23 - 2017" should "solve first part" in {
    Day23.part1(Day23.input) should equal(5929)
  }

  it should "solve second part" in {
    Day23.part2(Day23.input) should equal(907)
  }
}