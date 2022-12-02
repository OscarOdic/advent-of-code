package com.oodic.aoc.y2022

object Day01 extends Puzzle2022[List[List[Int]], Int, Int] {
  override val input: List[List[Int]] =
    getInputFile.foldLeft(List(List.empty[Int])) {
      case (list, "") => List.empty +: list
      case (head :: tail, value) => (head :+ value.toInt) :: tail
    }.reverse

  override def part1(input: List[List[Int]]): Int =
    input
      .map(_.sum)
      .max

  override def part2(input: List[List[Int]]): Int =
    input
      .map(_.sum)
      .sorted(Ordering.Int.reverse)
      .take(3)
      .sum
}
