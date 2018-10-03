package com.oodic.aoc.y2016

import org.scalatest.{FlatSpec, Matchers}

class Day09Test extends FlatSpec with Matchers {

  "Day09 - 2016" should "answer first part" in {
    Day09.resolveFirst("ADVENT") should be(6)
    Day09.resolveFirst("A(1x5)BC") should be(7)
    Day09.resolveFirst("(3x3)XYZ") should be(9)
    Day09.resolveFirst("A(2x2)BCD(2x2)EFG") should be(11)
    Day09.resolveFirst("(6x1)(1x3)A") should be(6)
    Day09.resolveFirst("X(8x2)(3x3)ABCY") should be(18)

    Day09.resolveFirst(Day09.input) should be(150914)
  }

  it should "answer second part" in {
    Day09.resolveSecond("(3x3)XYZ") should be(9)
    Day09.resolveSecond("X(8x2)(3x3)ABCY") should be(20)
    Day09.resolveSecond("(27x12)(20x12)(13x14)(7x10)(1x12)A") should be(241920)
    Day09.resolveSecond("(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN") should be(445)

    Day09.resolveSecond(Day09.input) should be(11052855125L)
  }
}
