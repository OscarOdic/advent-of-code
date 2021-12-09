package com.oodic.aoc.y2016

import scala.util.parsing.combinator.RegexParsers

object Day20 extends Puzzle2016[List[String], Long, Int] with RegexParsers {
  override val input: List[String] = getInputFile

  private val rangeParser = for {
    left <- """\d+""".r ^^ (_.toLong)
    right <- "-" ~> """\d+""".r ^^ (_.toLong)
  } yield (left, right)

  private def lowestAllowedIp(blackList: List[(Long, Long)]) = {
    val sortedBlackList = blackList.sortBy(_._1)
    def rec(value: Long = 0, i: Int = 0): Long = sortedBlackList(i) match {
      case (left, _) if value < left => value
      case (_, right) if value <= right => rec(right + 1, i + 1)
      case _ => rec(value, i + 1)
    }
    rec()
  }

  private def numAllowedIp(blackList: List[(Long, Long)], end: Long = 4294967295L) = {
    val sortedBlackList = blackList.sortBy(_._1)
    def rec(value: Long = 0, i: Int = 0, num: Int = 0): Int =
      if (value > end) num
      else sortedBlackList(i) match {
        case (left, _) if value < left => rec(value + 1, i, num + 1)
        case (_, right) if value <= right => rec(right + 1, i + 1, num)
        case _ => rec(value, i + 1, num)
      }
    rec()
  }

  override def part1(input: List[String]): Long =
    lowestAllowedIp(input.map(parse(rangeParser, _).get))

  override def part2(input: List[String]): Int =
    numAllowedIp(input.map(parse(rangeParser, _).get))
}