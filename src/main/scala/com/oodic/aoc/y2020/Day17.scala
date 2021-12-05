package com.oodic.aoc.y2020

import scala.annotation.tailrec

object Day17 extends Puzzle2020[List[String], Int, Int] {
  override val input: List[String] =
    getInputFile

  private def parseCells(cells: List[String], dim: Int = 3): Map[List[Int], Boolean] =
    (for {
      (line, y) <- cells.zipWithIndex
      (c, x) <- line.zipWithIndex
      if c == '#'
    } yield List(x, y) ++ List.fill(dim - 2)(0) -> true).toMap

  private def bounds(cells: Map[List[Int], Boolean]): List[(Int, Int)] = {
    cells.filter(_._2).keys.toList.transpose.map(dimension =>
      (dimension.min - 1, dimension.max + 1)
    )
  }

  def combs[A](xss: List[List[A]]): List[List[A]] =
    xss match {
      case Nil => List(Nil)
      case xs :: rss => for {
        x <- xs
        cs <- combs(rss)
      } yield x :: cs
    }

  private def neighbors(cell: List[Int]): List[List[Int]] =
    combs(cell.map(x => (x - 1 to x + 1).toList)).filter(!_.equals(cell))

  private def isActive(cell: List[Int], cells: Map[List[Int], Boolean]) = {
    val cellIsActive = cells.getOrElse(cell, false)
    val activeNeighbors = neighbors(cell).count(cells.getOrElse(_, false))
    (cellIsActive && List(2, 3).contains(activeNeighbors)) || (!cellIsActive && activeNeighbors == 3)
  }

  @tailrec
  private def execute(cells: Map[List[Int], Boolean], cycle: Int = 6): Map[List[Int], Boolean] = {
    if (cycle > 0)
      execute(combs(bounds(cells).map(x => (x._1 to x._2).toList))
        .foldLeft(cells)((currentCells, cell) =>
          currentCells.updated(cell, isActive(cell, cells))
        ), cycle - 1)
    else
      cells
  }

  override def resolveFirst(input: List[String]): Int =
    execute(parseCells(input)).count(_._2)

  override def resolveSecond(input: List[String]): Int =
    execute(parseCells(input, 4)).count(_._2)
}
