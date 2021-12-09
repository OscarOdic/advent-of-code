package com.oodic.aoc.y2016

import scala.util.parsing.combinator.RegexParsers

object Day04 extends Puzzle2016[List[String], Int, Int] with RegexParsers {
  override val input: List[String] = getInputFile

  case class Room(name: String, id: Int, checksum: String)

  private val roomParser: Parser[Room] = for {
    name <- rep1sep("""[a-z]+""".r, "-") ^^ (_.mkString)
    id <- "-" ~> """\d+""".r ^^ (_.toInt)
    checksum <- "[" ~> """\w+""".r <~ "]"
  } yield Room(name, id, checksum)

  private def mostCommonLetters(value: String) =
    value.groupBy(identity)
      .mapValues(_.length).toList
      .sortWith((x, y) =>
        x._2 > y._2 || (x._2 == y._2 && x._1 < y._1)
      ).take(5).map(_._1).mkString

  private def decode(value: String, key: Int) =
    value.toList.map(c => {
      val newValue = c.toInt + (key % 26)
      (if (newValue > 122) newValue - 26 else newValue).toChar
    }).mkString

  override def part1(input: List[String]): Int =
    input.map(parse(roomParser, _).get)
      .filter(room => mostCommonLetters(room.name) == room.checksum)
      .map(_.id).sum

  override def part2(input: List[String]): Int = {
    def rec(rooms: List[Room] = input.map(parse(roomParser, _).get)): Int = rooms match {
      case h::_ if decode(h.name, h.id).contains("northpole") => h.id
      case _::t => rec(t)
      case _ => 0
    }

    rec()
  }
}