package com.oodic.aoc.y2021

object Day01 extends Puzzle2021[List[Int], Int, Int] {
  override val input: List[Int] =
    getInputFile.map(_.toInt)

  private def numberIncreased(report: List[Int]): Int =
    report
      .sliding(2)
      .count { case x :: y :: _ => y > x }

  private def reportToWindows(report: List[Int]): List[Int] =
    report
      .sliding(3)
      .map(_.sum)
      .toList

  override def resolveFirst(input: List[Int]): Int =
    numberIncreased(input)

  override def resolveSecond(input: List[Int]): Int =
    numberIncreased(reportToWindows(input))
}
