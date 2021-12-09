package com.oodic.aoc.y2021

import scala.annotation.tailrec

object Day03 extends Puzzle2021[List[List[Int]], Int, Int] {
  override val input: List[List[Int]] =
    getInputFile.map(_.toList.map(_.asDigit))

  private def countBits(report: List[List[Int]], index: Int): (Int, Int) =
    report.foldLeft((0, 0)) {
      case ((one, zero), line) =>
        if (line(index) == 0) (one, zero + 1)
        else (one + 1, zero)
    }

  @tailrec
  private def power(report: List[List[Int]], index: Int = 0, gamma: String = "", epsilon: String = ""): Int =
    if (index < report.head.size) {
      val (one, zero) = countBits(report, index)
      power(report, index + 1, if (one > zero) gamma + "1" else gamma + "0", if (one > zero) epsilon + "0" else epsilon + "1")
    }
    else
      Integer.parseInt(gamma, 2) * Integer.parseInt(epsilon, 2)

  @tailrec
  private def rating(report: List[List[Int]], oxygen: Boolean, index: Int = 0): Int =
    if (report.size == 1) Integer.parseInt(report.head.mkString, 2)
    else {
      val (one, zero) = countBits(report, index)
      val filtered = report.filter(
        _(index) == (if (if (oxygen) one >= zero else one < zero) 1 else 0)
      )
      rating(filtered, oxygen, index + 1)
    }

  override def part1(input: List[List[Int]]): Int =
    power(input)

  override def part2(input: List[List[Int]]): Int = {
    val (oxygenGeneratorRating, co2ScrubberRating) = (
      rating(input, oxygen = true),
      rating(input, oxygen = false)
    )
    oxygenGeneratorRating * co2ScrubberRating
  }
}
