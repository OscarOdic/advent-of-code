package com.oodic.aoc

import scala.io.Source
import scala.util.parsing.combinator.RegexParsers

object Day13 extends RegexParsers {
  val input = Source.fromResource("day13.txt").getLines.toList

  val instructionParser: Parser[(Int, Int)] = for {
    key <- """\d+""".r ^^ (_.toInt)
    _ <- ":".r
    length <- """\d+""".r ^^ (_.toInt)
  } yield (key, length)

  def resolveFirst(input: List[String]): Int = input
    .map(parse(instructionParser, _).get)
    .map {
      case (key, value) =>  if (key % (2 * (value - 1)) == 0) key * value else 0
    }.sum

  def resolveSecond(input: List[String]): Int = {
    val instructions = input.map(parse(instructionParser, _).get)
    def rec(delay: Int = 0): Int =
      if (instructions.exists {case (key, value) => (key + delay) % (2 * (value - 1)) == 0 })
        rec(delay + 1)
      else
        delay
    rec()
  }

  def main(args: Array[String]): Unit = {
    println(s"[first star] ${resolveFirst(input)}")
    println(s"[second star] ${resolveSecond(input)}")
  }
}
