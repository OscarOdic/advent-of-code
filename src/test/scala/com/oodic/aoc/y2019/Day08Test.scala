package com.oodic.aoc.y2019

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day08Test extends AnyFlatSpec with Matchers {
  "Day08 - 2019" should "solve first part" in {
    Day08.part1(Day08.input) should equal(1820)
  }

  it should "solve second part" in {
    val testInput = List(
      "#### #  # #  #  ##    ## ",
      "   # #  # # #  #  #    # ",
      "  #  #  # ##   #       # ",
      " #   #  # # #  #       # ",
      "#    #  # # #  #  # #  # ",
      "####  ##  #  #  ##   ##  "
    )

    Day08.part2(Day08.input) should equal("\n" + testInput.mkString("\n"))
  }
}
