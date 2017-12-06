package adventOfCode

import scala.io.Source

object Day5 {
  val input = Source.fromResource("day5.txt").getLines.toVector.map(_.toInt)

  def resolveWithF(instructions: Vector[Int])(f: Int => Int) = {
    def rec(instructions: Vector[Int], index: Int = 0, nStep: Long = 0): Long = {
      val value = instructions(index)
      index + value match {
        case n if n >= instructions.length => nStep + 1
        case n => rec(instructions.updated(index, f(value)), n, nStep + 1)
      }
    }
    rec(instructions)
  }

  def resolveFirst(instructions: Vector[Int]) =
    resolveWithF(instructions)(_ + 1)

  def resolveSecond(instructions: Vector[Int]) =
    resolveWithF(instructions) {
      case value if value >= 3 => value - 1
      case value => value + 1
    }

  def main(args: Array[String]): Unit = {
    println(s"[first star] ${resolveFirst(input)}")
    println(s"[second star] ${resolveSecond(input)}")
  }
}
