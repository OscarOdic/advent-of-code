package com.oodic.aoc

object Day01 extends PuzzleDay[Seq[Int], Int, Int] {
  override val input: Seq[Int] = getInputFile.mkString.map(_.asDigit)

  def resolveWithN(list: Seq[Int], n: Int): Int = list.zipWithIndex.map {
    case (value, index) if index+n < list.size => (value, list(index+n))
    case (value, index) => (value, list(index + n - list.size))
  }.foldLeft(0) {
    case (acc, (left, right)) if left == right => acc + left
    case (acc, _) => acc
  }

  override def resolveFirst(list: Seq[Int]): Int = resolveWithN(list, 1)

  override def resolveSecond(list: Seq[Int]): Int = resolveWithN(list, list.size/2)
}
