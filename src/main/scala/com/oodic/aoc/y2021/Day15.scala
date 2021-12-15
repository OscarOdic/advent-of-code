package com.oodic.aoc.y2021

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object Day15 extends Puzzle2021[Map[(Int, Int), Int], Int, Int] {
  override val input: Map[(Int, Int), Int] =
    (for {
      (line, y) <- getInputFile.zipWithIndex
      (value, x) <- line.zipWithIndex
    } yield (x, y) -> value.asDigit).toMap

  def neighbors(cell: (Int, Int), cells: Map[(Int, Int), Int]): List[(Int, Int)] =
    List((cell._1 + 1, cell._2), (cell._1 - 1, cell._2), (cell._1, cell._2 - 1), (cell._1, cell._2 + 1))
      .filter(cells.contains)

  private def getMinPath(cells: Map[(Int, Int), Int]): Int = {
    @tailrec
    def rec(todo: Queue[(Int, Int)], risk: Map[(Int, Int), Int]): Map[(Int, Int), Int] = {
      todo.dequeueOption match {
        case Some((h, t)) =>
          val n = neighbors(h, cells)
          val (nextRisks, nextTodos) = n.foldLeft((risk, List.empty[(Int, Int)])) {
            case ((currentRisk, currentTodo), neighbor) =>
              if (!currentRisk.contains(neighbor) || currentRisk(h) + cells(neighbor) < currentRisk(neighbor))
                (currentRisk.updated(neighbor, currentRisk(h) + cells(neighbor)), currentTodo :+ neighbor)
              else
                (currentRisk, currentTodo)
          }
          rec(t ++ nextTodos, nextRisks)
        case _ => risk
      }
    }

    rec(Queue((0, 0)), Map((0, 0) -> 0))((cells.keys.map(_._1).max, cells.keys.map(_._2).max))
  }

  private def higherCave(cells:  Map[(Int, Int), Int]): Map[(Int, Int), Int] = {
    val (width, height) = (cells.keys.map(_._1).max + 1, cells.keys.map(_._2).max + 1)
    List.tabulate(5, 5)((x, y) =>
      cells.toSeq.map {
        case (cell, value) =>
          (x * width + cell._1, y * height + cell._2) -> (1 + (value - 1 + x + y) % 9)
      }
    ).flatten
      .flatten
      .toMap
  }

  override def part1(input: Map[(Int, Int), Int]): Int =
    getMinPath(input)

  override def part2(input: Map[(Int, Int), Int]): Int =
    getMinPath(higherCave(input))
}
