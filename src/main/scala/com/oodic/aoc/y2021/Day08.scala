package com.oodic.aoc.y2021

object Day08 extends Puzzle2021[List[(List[String], List[String])], Int, Int] {
  override val input: List[(List[String], List[String])] =
    getInputFile
      .map(line =>
        line.split(" \\| ") match {
          case Array(left, right) => (left.split(" ").toList, right.split(" ").toList)
        }
      )

  private def getSignalMapper(signals: List[String]): Map[String, Int] = {
    val mapUniqueNumber = Map(
      1 -> signals.find(_.length == 2).get,
      4 -> signals.find(_.length == 4).get,
      7 -> signals.find(_.length == 3).get,
      8 -> signals.find(_.length == 7).get
    )
    signals
      .map(s => (s.length, s))
      .filter(s => List(5, 6).contains(s._1))
      .foldLeft(mapUniqueNumber) {
        case (currentMap, (size, signal)) =>
          (size, (signal.intersect(currentMap(1)).length, signal.intersect(currentMap(4)).length)) match {
            case (6, (2, 3)) => currentMap.updated(0, signal)
            case (5, (1, 2)) => currentMap.updated(2, signal)
            case (5, (2, 3)) => currentMap.updated(3, signal)
            case (5, (1, 3)) => currentMap.updated(5, signal)
            case (6, (1, 3)) => currentMap.updated(6, signal)
            case (6, (2, 4)) => currentMap.updated(9, signal)
          }
      }.mapValues(_.sorted)
      .map(_.swap)
  }

  override def part1(input: List[(List[String], List[String])]): Int =
    input.map {
      case (_, right) => right.count(number => List(2, 3, 4, 7).contains(number.length))
    }.sum

  override def part2(input: List[(List[String], List[String])]): Int =
    input
      .map {
        case (left, right) =>
          val mapper = getSignalMapper(left)
          right
            .map(s => mapper(s.sorted))
            .mkString.toInt
      }.sum
}
