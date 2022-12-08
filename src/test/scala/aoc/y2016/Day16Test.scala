package aoc.y2016

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day16Test extends AnyFlatSpec with Matchers {
  "Day16 - 2016" should "solve first part" in {
    Day16.part1(Day16.input) should equal("10010101010011101")
  }

  it should "solve second part" in {
    Day16.part2(Day16.input) should equal("01100111101101111")
  }
}
