package com.oodic.aoc.y2020

import scala.annotation.tailrec

object Day11 extends Puzzle2020[Map[(Int, Int), Char], Int, Int] {
  type Layout = Map[(Int, Int), Char]

  override val input: Layout = (
    for {
      (value, y) <- getInputFile.zipWithIndex
      (char, x) <- value.zipWithIndex
    } yield ((x, y), char)).toMap

  private def adjacentPos(x: Int, y: Int): List[(Int, Int)] =
    List(
      (x + 1, y),
      (x + 1, y + 1),
      (x, y + 1),
      (x - 1, y + 1),
      (x - 1, y),
      (x - 1, y - 1),
      (x, y - 1),
      (x + 1, y - 1)
    )

  @tailrec
  private def visibleSeatDirection(layout: Layout, x: Int, y: Int, dirX: Int, dirY: Int): Char =
    layout.getOrElse((x + dirX, y + dirY), 'L') match {
      case char if char != '.' => char
      case _ => visibleSeatDirection(layout, x + dirX, y + dirY, dirX, dirY)
    }

  private def visibleSeatsAround(layout: Layout, x: Int, y: Int): List[Char] =
    List((1, 0), (1, 1), (0, 1), (-1, 1), (-1, 0), (-1, -1), (0, -1), (1, -1))
      .map {
        case (dirX, dirY) => visibleSeatDirection(layout, x, y, dirX, dirY)
      }

  @tailrec
  private def executeRule(layout: Layout, noChanges: Boolean = false)
                         (occupiedRule : (Layout, Int, Int) => Boolean, emptyRule: (Layout, Int, Int) => Boolean)
  : (Layout, Boolean) =
    if (noChanges)
      (layout, true)
    else {
      val (nextLayout, nextNoChanges) = layout.foldLeft((layout, true)) {
        case ((updatedLayout, noChange), ((x, y), '#')) =>
          if (occupiedRule(layout, x, y))
            (updatedLayout.updated((x, y), 'L'), false)
          else
            (updatedLayout, noChange)
        case ((updatedLayout, noChange), ((x, y), 'L')) =>
          if (emptyRule(layout, x, y))
            (updatedLayout.updated((x, y), '#'), false)
          else
            (updatedLayout, noChange)
        case ((updatedLayout, noChange), (_, '.')) =>
          (updatedLayout, noChange)
      }
      executeRule(nextLayout, nextNoChanges)(occupiedRule, emptyRule)
    }

  override def part1(input: Layout): Int =
    executeRule(input)(
      occupiedRule = (layout: Layout, x: Int, y: Int) =>
        adjacentPos(x, y).count(layout.getOrElse(_, 'L') == '#') >= 4,
      emptyRule = (layout: Layout, x: Int, y: Int) =>
        adjacentPos(x, y).forall(List('L', '.') contains layout.getOrElse(_, 'L'))
    )._1.count(_._2 == '#')

  override def part2(input: Layout): Int =
    executeRule(input)(
      occupiedRule = (layout: Layout, x: Int, y: Int) =>
        visibleSeatsAround(layout, x, y).count(_ == '#') >= 5,
      emptyRule = (layout: Layout, x: Int, y: Int) =>
        visibleSeatsAround(layout, x, y).forall(List('L', '.') contains _)
    )._1.count(_._2 == '#')
}
