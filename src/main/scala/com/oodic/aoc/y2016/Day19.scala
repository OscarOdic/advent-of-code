package com.oodic.aoc.y2016

object Day19 extends Puzzle2016[Int, Int, Int] {
  override val input: Int = 3001330

  private def takeRight(nbElves: Int) = {
    def rec(value: Int = 0): Int = if (math.pow(2, value) > nbElves) value - 1 else rec(value + 1)
    1 + 2 * (nbElves - math.pow(2, rec()).toInt)
  }

  private def takeAcross(nbElves: Int) = {
    val pow = math.pow(3, BigInt(nbElves).toString(3).length - 1).toInt
    if (pow == nbElves) pow
    else if (pow >= nbElves / 2) nbElves - pow
    else pow + (2 * (nbElves - 2 * pow))
  }

  override def part1(input: Int): Int = takeRight(input)

  override def part2(input: Int): Int = takeAcross(input)
}