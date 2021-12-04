package com.oodic.aoc.y2021

import org.scalatest.{FlatSpec, Matchers}

class Day04Test extends FlatSpec with Matchers {
  val testInput = (
    List(7, 4, 9, 5, 11, 17, 23, 2, 0, 14, 21, 24, 10, 16, 13, 6, 15, 25, 12, 22, 18, 20, 8, 19, 3, 26, 1),
    List(
    List(List(22, 13, 17, 11, 0),
      List(8, 2, 23, 4, 24),
      List(21, 9, 14, 16, 7),
      List(6, 10, 3, 18, 5),
      List(1, 12, 20, 15, 19)),

    List(List(3, 15, 0, 2, 22),
      List(9, 18, 13, 17, 5),
      List(19, 8, 7, 25, 23),
      List(20, 11, 10, 24, 4),
      List(14, 21, 16, 12, 6)),

    List(List(14, 21, 17, 24, 4),
      List(10, 16, 15, 9, 19),
      List(18, 8, 23, 26, 20),
      List(22, 11, 13, 6, 5),
      List(2, 0, 12, 3, 7))
    )
  )

  "Day04 - 2021" should "answer first part" in {
    Day04.resolveFirst(testInput) should be(4512)

    Day04.resolveFirst(Day04.input) should be(2745)
  }

  it should "answer second part" in {
    Day04.resolveSecond(testInput) should be(1924)

    Day04.resolveSecond(Day04.input) should be(6594)
  }
}