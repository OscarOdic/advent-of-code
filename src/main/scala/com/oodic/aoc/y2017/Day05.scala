package com.oodic.aoc.y2017

object Day05 extends Puzzle2017[Vector[Int], Long, Long] {
  override val input: Vector[Int] = getInputFile.toVector.map(_.toInt)

  def resolveWithF(instructions: Vector[Int])(f: Int => Int): Long = {
    def rec(instructions: Vector[Int], index: Int = 0, nStep: Long = 0): Long = {
      val value = instructions(index)
      index + value match {
        case n if n >= instructions.length => nStep + 1
        case n => rec(instructions.updated(index, f(value)), n, nStep + 1)
      }
    }
    rec(instructions)
  }

  override def part1(instructions: Vector[Int]): Long =
    resolveWithF(instructions)(_ + 1)

  override def part2(instructions: Vector[Int]): Long =
    resolveWithF(instructions) {
      case value if value >= 3 => value - 1
      case value => value + 1
    }
}
