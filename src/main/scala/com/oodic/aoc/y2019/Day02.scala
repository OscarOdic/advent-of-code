package com.oodic.aoc.y2019

object Day02 extends Puzzle2019[Vector[Int], Int, Int] {
  override  val input: Vector[Int] = getInputFile.head.split(",").toVector.map(_.toInt)

  private def execute(input: Vector[Int], index: Int = 0): Vector[Int] = input(index) match {
    case 99 => input
    case 1 => execute(input.updated(input(index + 3), input(input(index + 1)) + input(input(index + 2))), index + 4)
    case 2 => execute(input.updated(input(index + 3), input(input(index + 1)) * input(input(index + 2))), index + 4)
  }

  override def resolveFirst(input:  Vector[Int]): Int = execute(input.updated(1, 12).updated(2, 2))(0)

  override def resolveSecond(input:  Vector[Int]): Int = (for {
    noun <- 0 to 99
    verb <- 0 to 99
    if execute(input.updated(1, noun).updated(2, verb))(0) == 19690720
  } yield 100 * noun + verb).head
}
