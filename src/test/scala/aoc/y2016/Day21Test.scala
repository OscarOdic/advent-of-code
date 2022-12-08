package aoc.y2016

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day21Test extends AnyFlatSpec with Matchers {
  "Day21 - 2016" should "solve first part" in {
    Day21.part1(Day21.input) should equal("gcedfahb")
  }

  it should "solve second part" in {
    Day21.part2(Day21.input) should equal("hegbdcfa")
  }
}
