package com.oodic.aoc.y2019

import scala.util.parsing.combinator.RegexParsers

object Day01 extends Puzzle2019[List[Int], Int, Int] with RegexParsers {
  override val input: List[Int] = getInputFile.map(_.toInt)

  private def getFuel(mass: Int): Int = (mass / 3) - 2

  private def requiredFuel(mass: Int): Int = getFuel(mass) match {
    case fuel if fuel <= 0 => 0
    case fuel => requiredFuel(fuel) + fuel
  }

  override def part1(input: List[Int]): Int = input.map(getFuel).sum

  override def part2(input: List[Int]): Int = input.map(requiredFuel).sum
}
