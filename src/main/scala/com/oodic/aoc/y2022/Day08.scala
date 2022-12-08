package com.oodic.aoc.y2022

import scala.annotation.tailrec

object Day08 extends Puzzle2022[Map[(Int, Int), Int], Int, Int] {
  type Grid = Map[(Int, Int), Int]

  def toGrid(input: List[String]): Grid =
    input
      .map(_.map(_.asDigit).zipWithIndex)
      .zipWithIndex
      .flatMap {
        case (line, y) => line.map {
          case (value, x) => (x, y) -> value
        }
      }.toMap

  override val input: Grid = toGrid(getInputFile)

  private def isVisible(x: Int, y: Int, grid: Grid, xMax: Int, yMax: Int): Boolean = {
    val value = grid((x, y))
    val top = (0 until y).map((x, _))
    val bottom = (y + 1 to yMax).map((x, _))
    val left = (0 until x).map((_, y))
    val right = (x + 1 to xMax).map((_, y))

    List(top, bottom, left, right).exists(side =>
      side.forall {
        case (x, y) => grid((x, y)) < value
      }
    )
  }

  @tailrec
  private def treesVisibleFromList(trees: List[Int], viewing: Int, n: Int = 0): Int =
    trees match
      case head :: _ if head >= viewing => n + 1
      case _ :: tail => treesVisibleFromList(tail, viewing, n + 1)
      case _ => n

  private def treesVisibleScore(x: Int, y: Int, grid: Grid, xMax: Int, yMax: Int): Int = {
    val value = grid((x, y))
    val top = (0 to y).reverse.tail.map((x, _)).map(grid)
    val bottom = (y to yMax).tail.map((x, _)).map(grid)
    val left = (0 to x).reverse.tail.map((_, y)).map(grid)
    val right = (x to xMax).tail.map((_, y)).map(grid)

    List(top, bottom, left, right)
      .map(trees => treesVisibleFromList(trees.toList, value))
      .product
  }

  override def part1(input: Grid): Int = {
    val xMax = input.map(_._1._1).max
    val yMax = input.map(_._1._2).max
    input
      .keys
      .count {
        case (x, y) =>
          isVisible(x, y, input, xMax, yMax)
      }
  }

  override def part2(input: Grid): Int =
    val xMax = input.map(_._1._1).max
    val yMax = input.map(_._1._2).max
    input
      .keys
      .map {
        case (x, y) => treesVisibleScore(x, y, input, xMax, yMax)
      }.max
}
