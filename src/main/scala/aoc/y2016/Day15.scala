package aoc.y2016

import scala.util.parsing.combinator.RegexParsers

object Day15 extends Puzzle2016[List[String], Int, Int] with RegexParsers {
  override val input: List[String] = getInputFile

  case class Disc(nbPositions: Int, init: Int)

  private val parseDisc: Parser[Disc] = for {
    nbPositions <- """Disc #\d has""".r ~> """\d+""".r ^^ (_.toInt)
    init <- "positions; at time=0, it is at position" ~> """\d+""".r ^^ (_.toInt)
  } yield Disc(nbPositions, init)

  private def resolve(discs: List[Disc]): Int =
    Stream.iterate(0)(_ + 1).find(start =>
      discs.zipWithIndex.forall {
        case (disc, index) => (disc.init + index + start + 1) % disc.nbPositions == 0
      }
    ).getOrElse(-1)

  override def part1(input: List[String]): Int =
    resolve(input.map(parse(parseDisc, _).get))

  override def part2(input: List[String]): Int =
    resolve(input.map(parse(parseDisc, _).get) :+ Disc(11, 0))
}