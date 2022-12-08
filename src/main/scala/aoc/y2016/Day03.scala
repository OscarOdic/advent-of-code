package aoc.y2016

import scala.util.parsing.combinator.RegexParsers


object Day03 extends Puzzle2016[List[String], Int, Int] with RegexParsers {
  override val input: List[String] = getInputFile

  private val triangleParser: Parser[(Int, Int, Int)] = for {
    side1 <- """\d+""".r ^^ (_.toInt)
    side2 <- """\d+""".r ^^ (_.toInt)
    side3 <- """\d+""".r ^^ (_.toInt)
  } yield (side1, side2, side3)

  private def isValid(triangle: (Int, Int, Int)) =
    List(triangle._1, triangle._2, triangle._3).sorted(Ordering[Int].reverse) match {
      case h::t => t.sum > h
    }

  private def convert(triangles: List[(Int, Int, Int)]): List[(Int, Int, Int)] =
    triangles.grouped(3).toList
      .flatMap(group => List(group.map(_._1), group.map(_._2), group.map(_._3))) map {
        case List(x, y, z) => (x, y, z)
      }

  override def part1(input: List[String]): Int =
    input.map(parse(triangleParser, _).get).count(isValid)

  override def part2(input: List[String]): Int =
    convert(input.map(parse(triangleParser, _).get)).count(isValid)
}
