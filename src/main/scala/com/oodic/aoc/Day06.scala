package com.oodic.aoc

import scala.io.Source

object Day06 {
  val input = Source.fromResource("day6.txt").getLines.mkString.split("\t").toVector.map(_.toInt)

  def getCycle(history: List[Vector[Int]], value: Vector[Int]) =
    history.zipWithIndex.find(_._1 equals value).map(_._2 + 1).getOrElse(0)

  def resolve(bank: Vector[Int]): (Int, Int) = {
    def rec(bank: Vector[Int], history: List[Vector[Int]] = List(), nStep: Int = 0): (Int, Int) = {
      def update(list: Vector[Int], values: Int, initialPos: Int): Vector[Int] = values match {
        case 0 => list
        case n => update(
          list.updated(initialPos, list(initialPos) + 1),
          values - 1,
          if (initialPos != list.size - 1) initialPos + 1 else 0
        )
      }
      if (history.contains(bank)) {
        (nStep, getCycle(history, bank))
      }
      else {
        val maxValue = bank.zipWithIndex.tail.foldLeft((0,bank.head)) {
          case ((_, max), (blocks, index)) if blocks > max => (index, blocks)
          case (acc, _) => acc
        }
        val updatedBank = update(
          bank.updated(maxValue._1, 0),
          maxValue._2,
          if (maxValue._1 != bank.size - 1) maxValue._1 + 1 else 0
        )
        rec(updatedBank, bank +: history, nStep + 1)
      }
    }
    rec(bank)
  }

  def main(args: Array[String]): Unit = {
    val resolved = resolve(input)
    println(s"[first star] ${resolved._1}")
    println(s"[second star] ${resolved._2}")
  }
}
