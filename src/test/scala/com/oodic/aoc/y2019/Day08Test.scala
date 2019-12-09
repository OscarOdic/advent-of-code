package com.oodic.aoc.y2019

import org.scalatest.{FlatSpec, Matchers}

class Day08Test extends FlatSpec with Matchers {
  "Day08 - 2019" should "answer first part" in {
    Day08.resolveFirst(Day08.input) should be(1820)
  }

  it should "answer second part" in {
    val testInput = List(
      "#### #  # #  #  ##    ## ",
      "   # #  # # #  #  #    # ",
      "  #  #  # ##   #       # ",
      " #   #  # # #  #       # ",
      "#    #  # # #  #  # #  # ",
      "####  ##  #  #  ##   ##  "
    )

    Day08.resolveSecond(Day08.input) should be("\n" + testInput.mkString("\n"))
  }
}
