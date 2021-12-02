package com.oodic.aoc.y2020

object Day06 extends Puzzle2020[List[List[String]], Int, Int] {
  override val input: List[List[String]] =
    getInputFile.foldLeft(List(List.empty[String])) ((list, line) =>
      if (line != "") list match {
        case h :: t => (h :+ line) :: t
      }
      else
        List.empty[String] +: list
    ).reverse

  override def resolveFirst(input: List[List[String]]): Int =
    input
      .map(_.flatten.distinct.size)
      .sum

  override def resolveSecond(input: List[List[String]]): Int =
    input
      .map(_.reduce(_ intersect _).length)
      .sum
}
