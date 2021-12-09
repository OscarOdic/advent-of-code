package com.oodic.aoc.y2017

import scala.util.parsing.combinator.RegexParsers

object Day16 extends Puzzle2017[List[String], String, String] with RegexParsers {
  override val input: List[String] = getInputFile.mkString.split(",").toList

  val defaultPrograms = "abcdefghijklmnop"

  trait Move
  case class Spin(value: Int) extends Move
  case class Exchange(a: Int, b: Int) extends Move
  case class Partner(a: Char, b: Char) extends Move

  val spinParser: Parser[Spin] = for {
    value <- "s" ~> """\d+""".r ^^ (_.toInt)
  } yield Spin(value)

  val exchangeParser: Parser[Exchange] = for {
    _ <- "x".r
    a <- """\d+""".r ^^ (_.toInt)
    _ <- "/".r
    b <- """\d+""".r ^^ (_.toInt)
  } yield Exchange(a, b)

  val partnerParser: Parser[Partner] = for {
    _ <- "p".r
    a <- ".".r ^^ (_.head)
    _ <- "/".r
    b <- ".".r ^^ (_.head)
  } yield Partner(a, b)

  val moveParser: Parser[Move] = spinParser | exchangeParser | partnerParser

  def resolveWithPrograms(input: List[String], programs: String = defaultPrograms): String =
    input.map(parse(moveParser, _).get).foldLeft(programs.toList) {
      case (accPrograms, Spin(value)) =>
        val split = accPrograms.splitAt(accPrograms.length - value)
        split._2 ++ split._1
      case (accPrograms, Exchange(a, b)) =>
        accPrograms
          .updated(a, accPrograms(b))
          .updated(b, accPrograms(a))
      case (accPrograms, Partner(a, b)) =>
        accPrograms.map {
          case char if char == a => b
          case char if char == b => a
          case other => other
        }
    }.mkString

  override def part1(input: List[String]): String =
    resolveWithPrograms(input)

  override def part2(input: List[String]): String =
    (0 until 1000000000).foldLeft(defaultPrograms, Map.empty[String, String]) {
      case ((programs, sequence), i) =>
        sequence.get(programs) match {
          case Some(newPrograms) => (newPrograms, sequence)
          case None =>
            val newPrograms = resolveWithPrograms(input, programs)
            (newPrograms, sequence.updated(programs, newPrograms))
        }
    }._1
}
