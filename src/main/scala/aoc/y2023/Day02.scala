package aoc.y2023

import aoc.y2023.Day02.Cube

import scala.collection.immutable.Map
import scala.util.parsing.combinator.RegexParsers

object Day02 extends Puzzle2023[List[String], Int, Int] with RegexParsers {
  val input: List[String] = getInputFile

  private val defaultBag: Map[Cube, Int] = Map(
    Cube.Red -> 12,
    Cube.Green -> 13,
    Cube.Blue -> 14,
  )

  private enum Cube:
    case Red, Green, Blue

  private case class Grab(cubes: Map[Cube, Int]):
    def updateMinBag(bag: Map[Cube, Int]): Map[Cube, Int] =
      cubes.foldLeft(bag)((accBag, cube) =>
        if (accBag.getOrElse(cube._1, 0) < cube._2) accBag.updated(cube._1, cube._2)
        else accBag
      )

  private case class Game(id: Int, grabs: List[Grab]):
    def possible(contained: Map[Cube, Int]): Boolean =
      grabs.flatMap(_.cubes).forall { case (cube, n) =>
        n <= contained(cube)
      }

    def minBag: Map[Cube, Int] = {
      grabs.foldLeft(Map.empty[Cube, Int])((bag, grab) =>
        grab.updateMinBag(bag)
      )
    }

  private def cubeParser: Parser[Cube] =
    """red""".r ^^ (_ => Cube.Red) | """green""".r ^^ (_ => Cube.Green) | """blue""".r ^^ (_ => Cube.Blue)

  private def grabParser: Parser[Grab] =
    rep1sep(
      for {
        n <- """\d+""".r ^^ (_.toInt)
        cube <- cubeParser
      } yield cube -> n,
      ","
    ) ^^ (list => Grab(list.toMap))

  private def gameParser: Parser[Game] =
    for {
      id <- "Game" ~> """\d+""".r <~ ":" ^^ (_.toInt)
      grabs <- rep1sep(
        grabParser,
        ";"
      )
    } yield Game(id, grabs)

  def part1(input: List[String]): Int =
    input
      .map(parse(gameParser, _).get)
      .filter(_.possible(defaultBag))
      .map(_.id)
      .sum

  def part2(input: List[String]): Int =
    input
      .map(parse(gameParser, _).get)
      .map(_.minBag)
      .map(_.values.product)
      .sum
}
