package aoc.y2023

import scala.util.parsing.combinator.RegexParsers

object Day06 extends Puzzle2023[List[String], Long, Long] with RegexParsers {
  override val input: List[String] = getInputFile

  private case class Race(time: Long, distance: Long) {
    private def isWin(hold: Long): Boolean = (time - hold) * hold > distance
    def numberWin: Long = (0L to time).count(isWin)
  }

  private def timesParser: Parser[List[Long]] = "Time:" ~> rep1("""\d+""".r ^^ (_.toLong))
  private def distancesParser: Parser[List[Long]] = "Distance:" ~> rep1("""\d+""".r ^^ (_.toLong))

  private def parseRaces(input: List[String]): List[Race] =
    input match
      case List(timesInput, distancesInput) =>
        val times = parse(timesParser, timesInput).get
        val distances = parse(distancesParser, distancesInput).get
        times
          .zip(distances)
          .map { case (time, distance) => Race(time, distance) }

  override def part1(input: List[String]): Long =
    parseRaces(input)
      .map(_.numberWin)
      .product

  override def part2(input: List[String]): Long =
    parseRaces(input)
      .reduceLeft((x, y) => Race(
        (x.time.toString + y.time.toString).toLong,
        (x.distance.toString + y.distance.toString).toLong
      )).numberWin
}
