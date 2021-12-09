package com.oodic.aoc.y2017

object Day01 extends Puzzle2017[Seq[Int], Int, Int] {
  override val input: Seq[Int] = getInputFile.mkString.map(_.asDigit)

  def resolveWithN(list: Seq[Int], n: Int): Int = list.zipWithIndex.map {
    case (value, index) if index+n < list.size => (value, list(index+n))
    case (value, index) => (value, list(index + n - list.size))
  }.foldLeft(0) {
    case (acc, (left, right)) if left == right => acc + left
    case (acc, _) => acc
  }

  override def part1(list: Seq[Int]): Int = resolveWithN(list, 1)

  override def part2(list: Seq[Int]): Int = resolveWithN(list, list.size/2)
}
