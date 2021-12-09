package com.oodic.aoc.y2016

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day09Test extends AnyFlatSpec with Matchers {

  "Day09 - 2016" should "solve first part" in {
    Day09.part1("ADVENT") should equal(6)
    Day09.part1("A(1x5)BC") should equal(7)
    Day09.part1("(3x3)XYZ") should equal(9)
    Day09.part1("A(2x2)BCD(2x2)EFG") should equal(11)
    Day09.part1("(6x1)(1x3)A") should equal(6)
    Day09.part1("X(8x2)(3x3)ABCY") should equal(18)

    Day09.part1(Day09.input) should equal(150914)
  }

  it should "solve second part" in {
    Day09.part2("(3x3)XYZ") should equal(9)
    Day09.part2("X(8x2)(3x3)ABCY") should equal(20)
    Day09.part2("(27x12)(20x12)(13x14)(7x10)(1x12)A") should equal(241920)
    Day09.part2("(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN") should equal(445)

    Day09.part2(Day09.input) should equal(11052855125L)
  }
}
