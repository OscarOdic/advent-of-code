package aoc.y2016

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day20Test extends AnyFlatSpec with Matchers {
  "Day20 - 2016" should "solve first part" in {
    Day20.part1(Day20.input) should equal(14975795L)
  }

  it should "solve second part" in {
    Day20.part2(Day20.input) should equal(101)
  }
}
