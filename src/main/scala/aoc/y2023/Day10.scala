package aoc.y2023

import scala.annotation.tailrec

object Day10 extends Puzzle2023[List[String], Int, Int] {
  override val input: List[String] = getInputFile

  private type Grid = Map[(Int, Int), Char]
  private type GridWithDistance = Map[(Int, Int), (Char, Option[Int])]

  private trait Direction {
    val charsConnected: List[Char]

    def getPosition(from: (Int, Int)): (Int, Int)

    def isConnected(from: (Int, Int), grid: Grid): Boolean =
      charsConnected.contains(grid.getOrElse(getPosition(from), '.'))
  }
  private object DirLeft extends Direction {
    override val charsConnected: List[Char] = List('L', 'F', '-')
    override def getPosition(from: (Int, Int)): (Int, Int) = (from._1 - 1, from._2)
  }
  private object DirRight extends Direction {
    override val charsConnected: List[Char] = List('7', 'J', '-')
    override def getPosition(from: (Int, Int)): (Int, Int) = (from._1 + 1, from._2)
  }
  private object DirUp extends Direction {
    override val charsConnected: List[Char] = List('F', '7', '|')
    override def getPosition(from: (Int, Int)): (Int, Int) = (from._1, from._2 - 1)
  }
  private object DirDown extends Direction {
    override val charsConnected: List[Char] = List('J', 'L', '|')
    override def getPosition(from: (Int, Int)): (Int, Int) = (from._1, from._2 + 1)
  }

  private def toMap(input: List[String]): Grid =
    input
      .zipWithIndex
      .flatMap { case (line, y) =>
        line.zipWithIndex.map { case (value, x) => ((x, y), value) }
      }.toMap

  private def start(grid: Grid): (Int, Int) =
    grid.find(_._2 == 'S').get._1

  private def replaceStart(grid: Grid, startPos: (Int, Int)): Grid =
    grid.updated(
      startPos,
      List(DirLeft, DirRight, DirUp, DirDown)
        .filter(_.isConnected(startPos, grid)) match
          case List(DirLeft, DirRight) => '-'
          case List(DirDown, DirUp) => '|'
          case List(DirRight, DirUp) => 'L'
          case List(DirLeft, DirUp) => 'J'
          case List(DirLeft, DirDown) => '7'
          case List(DirRight, DirDown) => 'F'
    )

  private def connected(grid: GridWithDistance, pos: (Int, Int)): List[(Int, Int)] =
    grid(pos) match
      case ('|', _) => List(DirDown.getPosition(pos), DirUp.getPosition(pos))
      case ('-', _) => List(DirLeft.getPosition(pos), DirRight.getPosition(pos))
      case ('7', _) => List(DirLeft.getPosition(pos), DirDown.getPosition(pos))
      case ('J', _) => List(DirUp.getPosition(pos), DirLeft.getPosition(pos))
      case ('L', _) => List(DirUp.getPosition(pos), DirRight.getPosition(pos))
      case ('F', _) => List(DirRight.getPosition(pos), DirDown.getPosition(pos))

  @tailrec
  private def addDistances(
                           grid: GridWithDistance,
                           positionToCheck: List[((Int, Int), Int)]
                         ): GridWithDistance =
    positionToCheck match
      case Nil => grid
      case head :: tail =>
        grid(head._1) match
          case (_, Some(value)) if value <= head._2 => addDistances(grid, tail)
          case (char, _) => addDistances(
            grid.updated(head._1, (char, Some(head._2))),
            tail ++ connected(grid, head._1).map(position => (position, head._2 + 1))
          )

  private def getTilesIn(grid: GridWithDistance): Int =
    (0 to grid.map(_._1._2).max)
      .map(y =>
        grid
          .filter(_._1._2 == y)
          .toList.sortBy(_._1._1)
          .foldLeft((false, 0)) {
            case ((inside, count), (_ , ('|' | 'L' | 'J', Some(_)))) => (!inside, count)
            case ((inside, count), (_, ('.', _) | (_, None))) => (inside, count + (if (inside) 1 else 0))
            case ((inside, count), _) => (inside, count)
          }._2
      ).sum

  override def part1(input: List[String]): Int = {
    val grid = toMap(input)
    val startPos = start(grid)
    addDistances(
      replaceStart(grid, startPos).map { case (pos, char) => (pos, (char, None))},
      List((start(grid), 0))
    )
      .flatMap(_._2._2)
      .max
  }

  override def part2(input: List[String]): Int = {
    val grid = toMap(input)
    val startPos = start(grid)
    val gridWithDistance = addDistances(
      replaceStart(grid, startPos).map { case (pos, char) => (pos, (char, None)) },
      List((start(grid), 0))
    )
    getTilesIn(gridWithDistance)
  }
}
