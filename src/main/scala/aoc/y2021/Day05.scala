package aoc.y2021

import scala.util.parsing.combinator.RegexParsers

object Day05 extends Puzzle2021[List[String], Int, Int] with RegexParsers {
  override val input: List[String] =
    getInputFile

  case class Vent(left: (Int, Int), right: (Int, Int))

  private val parseVent = for {
    x1 <- """\d+""".r <~ "," ^^ (_.toInt)
    y1 <- """\d+""".r <~ "->" ^^ (_.toInt)
    x2 <- """\d+""".r <~ "," ^^ (_.toInt)
    y2 <- """\d+""".r ^^ (_.toInt)
  } yield Vent((x1, y1), (x2, y2))

  private def allCombinations(vent: Vent): List[(Int, Int)] =
    vent match {
      case Vent((x1, y1), (x2, y2)) =>
        val x = math.min(x1, x2) to math.max(x1, x2)
        val y = math.min(y1, y2) to math.max(y1, y2)
        (if (x1 <= x2) x else x.reverse)
          .zipAll(if (y1 <= y2) y else y.reverse,
            x1, y1).toList
    }

  override def part1(input: List[String]): Int =
    input
      .map(parse(parseVent, _).get)
      .filter {
        case Vent((x1, y1), (x2, y2)) => x1 == x2 || y1 == y2
      }.flatMap(allCombinations)
      .groupBy(identity)
      .count(_._2.size >= 2)

  override def part2(input: List[String]): Int =
    input
      .map(parse(parseVent, _).get)
      .flatMap(allCombinations)
      .groupBy(identity)
      .count(_._2.size >= 2)
}
