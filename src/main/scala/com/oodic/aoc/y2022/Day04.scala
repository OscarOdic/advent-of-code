package com.oodic.aoc.y2022

import scala.util.parsing.combinator.RegexParsers

object Day04 extends Puzzle2022[List[String], Int, Int] with RegexParsers {
  override val input: List[String] = getInputFile

  private case class Section(start: Int, end: Int) {
    def toList: List[Int] = (start to end).toList
  }

  private def sectionParser: Parser[Section] =
    for {
      start <- """\d*""".r ^^ (_.toInt)
      _ <- "-"
      end <- """\d*""".r ^^ (_.toInt)
    } yield Section(start, end)

  private def pairParser: Parser[(Section, Section)] =
    for {
      left <- sectionParser
      _ <- ","
      right <- sectionParser
    } yield (left, right)

  override def part1(input: List[String]): Int =
    input
      .map(parse(pairParser, _).get)
      .count {
        case (left, right) =>
          (left.start >= right.start && left.end <= right.end) || (left.start <= right.start && left.end >= right.end)
      }

  override def part2(input: List[String]): Int =
    input
      .map(parse(pairParser, _).get)
      .count {
        case (left, right) =>
          (left.toList intersect right.toList).nonEmpty
      }
}
