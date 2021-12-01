package com.oodic.aoc.y2020

object Day01 extends Puzzle2020[List[Int], Int, Int] {
  override val input: List[Int] =
    getInputFile.map(_.toInt)

  private def resolve(input: List[Int], n: Int): Int =
    input
      .combinations(n)
      .find(_.sum == 2020)
      .map(_.product)
      .getOrElse(0)

  override def resolveFirst(input: List[Int]): Int =
    resolve(input, 2)

  override def resolveSecond(input: List[Int]): Int =
    resolve(input, 3)
}
