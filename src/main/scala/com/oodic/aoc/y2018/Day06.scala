package com.oodic.aoc.y2018

import scala.util.parsing.combinator.RegexParsers

object Day06 extends Puzzle2018[List[String], Int, Int] with RegexParsers {
  override val input: List[String] = getInputFile

  private val coordsParser: Parser[(Int, Int)] = for {
    x <- """\d+""".r ^^ (_.toInt)
    y <- "," ~> """\d+""".r  ^^ (_.toInt)
  } yield (x, y)

  private def minMax(coords: List[(Int, Int)]) =
    (coords.map(_._1).min, coords.map(_._1).max, coords.map(_._2).min, coords.map(_._2).max)

  private def manhattanDistance(coord1: (Int, Int), coord2: (Int, Int)) =
    math.abs(coord1._1 - coord2._1) + math.abs(coord1._2 - coord2._2)

  private def getMap(minX: Int, maxX: Int, minY: Int, maxY: Int) = for {
    x <- minX to maxX
    y <- minY to maxY
  } yield (x, y)

  private def areasCount(allCoords: Seq[(Int, Int)], inputCoords: List[(Int, Int)]) = allCoords
    .map(c =>
      inputCoords.filter(manhattanDistance(c, _) == inputCoords.map(manhattanDistance(c, _)).min) match {
        case List(coord) => c -> Some(coord)
        case _ => c -> None
      }
    ).groupBy(_._2).mapValues(_.size)
    .filter(_._1.isDefined)

  override def resolveFirst(input: List[String]): Int = {
    val coords = input.map(parse(coordsParser, _).get)
    val (minX, maxX, minY, maxY) = minMax(coords)

    areasCount(getMap(minX, maxX, minY, maxY), coords)
      .zip(areasCount(getMap(minX - 1, maxX + 1, minY - 1, maxY + 1), coords))
      .filter(t => t._1._2 == t._2._2).keys.map(_._2).max
  }

  override def resolveSecond(input: List[String]): Int = {
    val coords = input.map(parse(coordsParser, _).get)
    val (minX, maxX, minY, maxY) = minMax(coords)
    getMap(minX, maxX, minY, maxY).count(c1 => coords.map(manhattanDistance(c1, _)).sum < 10000)
  }
}
