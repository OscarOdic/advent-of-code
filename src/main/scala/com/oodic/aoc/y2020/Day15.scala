package com.oodic.aoc.y2020

object Day15 extends Puzzle2020[List[Int], Int, Int] {
  override val input: List[Int] = List(1, 0, 18, 10, 19, 6)

  private def initSpoken(input: List[Int]): Map[Int, Int] =
    input.zipWithIndex.map {
      case (number, index) => (number, index + 1)
    }.toMap

  private def play(numbersSpoken: Map[Int, Int], start: Int, max: Int = 2020) =
    (start until max)
      .foldLeft((numbersSpoken, 0)) {
        case ((allNumbersSpoken, lastNumberSpoken), index) =>
          (allNumbersSpoken.updated(lastNumberSpoken, index),
            allNumbersSpoken.get(lastNumberSpoken) match {
              case Some(value) => index - value
              case _ => 0
            })
      }._2

  override def resolveFirst(input: List[Int]): Int =
    play(initSpoken(input), input.size + 1)

  override def resolveSecond(input: List[Int]): Int =
    play(initSpoken(input), input.size + 1, 30000000)
}
