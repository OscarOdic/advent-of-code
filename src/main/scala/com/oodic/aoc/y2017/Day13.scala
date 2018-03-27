package com.oodic.aoc.y2017

import scala.util.parsing.combinator.RegexParsers

object Day13 extends Puzzle2017[List[String], Int, Int] with RegexParsers {
  override val input: List[String] = getInputFile

  val instructionParser: Parser[(Int, Int)] = for {
    key <- """\d+""".r ^^ (_.toInt)
    _ <- ":".r
    length <- """\d+""".r ^^ (_.toInt)
  } yield (key, length)

  override def resolveFirst(input: List[String]): Int = input
    .map(parse(instructionParser, _).get)
    .map {
      case (key, value) =>  if (key % (2 * (value - 1)) == 0) key * value else 0
    }.sum

  override def resolveSecond(input: List[String]): Int = {
    val instructions = input.map(parse(instructionParser, _).get)
    def rec(delay: Int = 0): Int =
      if (instructions.exists {case (key, value) => (key + delay) % (2 * (value - 1)) == 0 })
        rec(delay + 1)
      else
        delay
    rec()
  }
}
