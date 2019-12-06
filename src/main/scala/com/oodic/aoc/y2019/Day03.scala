package com.oodic.aoc.y2019

import scala.util.parsing.combinator.RegexParsers

object Day03 extends Puzzle2019[Vector[List[String]], Int, Int] with RegexParsers {
  override val input: Vector[List[String]] = getInputFile.map(_.split(",").toList).toVector

  private trait Direction
  private object Right extends Direction
  private object Left extends Direction
  private object Up extends Direction
  private object Down extends Direction

  private case class Operation(direction: Direction, distance: Int)

  private val operationParser = for {
    direction <- """[RLUD]""".r
    distance <- """\d+""".r ^^ (_.toInt)
  } yield direction match {
    case "R" => Operation(Right, distance)
    case "L" => Operation(Left, distance)
    case "U" => Operation(Up, distance)
    case _ => Operation(Down, distance)
  }

  private def getPoints(operations: List[Operation]): List[(Int, Int)] =
    operations.foldLeft((List.empty[(Int, Int)], (0,0))) { case ((points, initialPoint), operation) =>
      operation.direction match {
        case Right =>
          val x = initialPoint._1 + operation.distance
          (points ::: (initialPoint._1 + 1 to x).map((_, initialPoint._2)).toList , initialPoint.copy(_1 = x))
        case Left =>
          val x = initialPoint._1 - operation.distance
          (points ::: (x until initialPoint._1).map((_, initialPoint._2)).reverse.toList , initialPoint.copy(_1 = x))
        case Up =>
          val y = initialPoint._2 + operation.distance
          (points ::: (initialPoint._2 + 1 to y).map((initialPoint._1, _)).toList , initialPoint.copy(_2 = y))
        case Down =>
          val y = initialPoint._2 - operation.distance
          (points ::: (y until initialPoint._2).map((initialPoint._1, _)).reverse.toList , initialPoint.copy(_2 = y))
      }
    }._1

  private def manhattanDistance(point: (Int, Int)): Int = math.abs(point._1) + math.abs(point._2)

  private def closetDistance(points: List[(Int, Int)]): Int =
    points.tail.foldLeft(manhattanDistance(points.head))((min, point) => manhattanDistance(point) match {
      case distance if distance < min => distance
      case _ => min
    })

  override def resolveFirst(input: Vector[List[String]]): Int = {
    val firstMove = getPoints(input(0).map(parse(operationParser, _).get))
    val secondMove = getPoints(input(1).map(parse(operationParser, _).get))
    closetDistance(firstMove intersect secondMove)
  }

  override def resolveSecond(input: Vector[List[String]]): Int = {
    val firstMove = getPoints(input(0).map(parse(operationParser, _).get))
    println(firstMove)
    val secondMove = getPoints(input(1).map(parse(operationParser, _).get))
    (firstMove intersect secondMove).map(i => firstMove.indexOf(i) + secondMove.indexOf(i)).min + 2
  }
}
