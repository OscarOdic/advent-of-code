package aoc.y2020

import scala.util.parsing.combinator.RegexParsers

object Day02 extends Puzzle2020[List[String], Int, Int] with RegexParsers {
  override val input: List[String] =
    getInputFile

  private case class Password(policyNumber1: Int, policyNumber2: Int, letter: Char, pass: String)

  private val passwordParser = for {
    policyNumber1 <- """\d+""".r ^^ (_.toInt)
    policyNumber2 <- "-" ~> """\d+""".r ^^ (_.toInt)
    letter <- """\w""".r <~ ":" ^^ (_.head)
    password <- """.*""".r
  } yield Password(policyNumber1, policyNumber2, letter, password)

  override def part1(input: List[String]): Int =
    input
      .map(parse(passwordParser, _).get)
      .count {
        case Password(min, max, letter, password) =>
          val countLetter = password.count(_ == letter)
          countLetter >= min && countLetter <= max
      }

  override def part2(input: List[String]): Int =
    input
      .map(parse(passwordParser, _).get)
      .count {
        case Password(pos1, pos2, letter, password) =>
          val letterPos1 = password.charAt(pos1 - 1)
          val letterPos2 = password.charAt(pos2 - 1)
          (letterPos1 == letter && letterPos2 != letter) || (letterPos1 != letter && letterPos2 == letter)
      }
}
