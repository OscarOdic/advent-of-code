package com.oodic.aoc.y2021

import org.scalatest.{FlatSpec, Matchers}

class Day09Test extends FlatSpec with Matchers {
  val testInput = List(
    List(2, 1, 9, 9, 9, 4, 3, 2, 1, 0),
    List(3, 9, 8, 7, 8, 9, 4, 9, 2, 1),
    List(9, 8, 5, 6, 7, 8, 9, 8, 9, 2),
    List(8, 7, 6, 7, 8, 9, 6, 7, 8, 9),
    List(9, 8, 9, 9, 9, 6, 5, 6, 7, 8)
  ).zipWithIndex
    .flatMap {
      case (line, y) =>
        line
          .zipWithIndex
          .map {
            case (v, x) =>
              (x, y) -> v
          }
    }.toMap

  "Day09 - 2021" should "answer first part" in {
    Day09.resolveFirst(testInput) should be(15)

    Day09.resolveFirst(Day09.input) should be(562)
  }

  it should "answer second part" in {
    Day09.resolveSecond(testInput) should be(1134)

    Day09.resolveSecond(Day09.input) should be(1076922)
  }
}
