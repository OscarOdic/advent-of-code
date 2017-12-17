package com.oodic.aoc

object Day17 extends PuzzleDay[Int, Int, Int] {
  override val input = 370

  override def resolveFirst(input: Int): Int = {
    val (finalList, finalPosition) = (1 to 2017).foldLeft((List(0), 0)) {
      case ((list, position), value) =>
        val newPosition = (position + input) % list.size
        val split = list.splitAt(newPosition + 1)
        ((split._1 :+ value) ++ split._2, newPosition + 1)
    }
    finalList(finalPosition + 1)
  }

  override def resolveSecond(input: Int): Int =
    (1 to 50000000).foldLeft((0, -1)) {
      case ((position, afterZero), value) =>
        val newPosition = (position + input) % value + 1
        (newPosition, if (newPosition == 1) value else afterZero)
    }._2
}
