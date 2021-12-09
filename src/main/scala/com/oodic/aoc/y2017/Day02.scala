package com.oodic.aoc.y2017

object Day02 extends Puzzle2017[List[List[Int]], Int, Int] {
  override val input: List[List[Int]] = getInputFile.map(_.split("\t").map(_.toInt).toList)

  def resolveWithF(input: List[List[Int]])(f: List[Int] => Int): Int = input.map(f).sum

  override def part1(input: List[List[Int]]): Int = resolveWithF(input) { l =>
    val min = l.min
    val max = l.max
    max - min
  }

  override def part2(input: List[List[Int]]): Int = resolveWithF(input)(l =>
    l.combinations(2).find {
      case List(first, second) => (first % second == 0) || (second % first == 0)
    }.map {
      case List(first, second) => if (first > second) first/second else second/first
    }.getOrElse(0)
  )
}
