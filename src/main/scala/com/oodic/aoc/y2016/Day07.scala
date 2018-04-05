package com.oodic.aoc.y2016

import scala.util.parsing.combinator.RegexParsers

object Day07 extends Puzzle2016[List[String], Int, Int] with RegexParsers {
  override val input: List[String] = getInputFile

  case class IP(inside: List[String], outside: List[String])

  private val ipParser: Parser[IP] = for {
    list <- rep1(
      for {
        outside <- """\w+""".r
        inside <- "[" ~> """\w+""".r <~ "]"
      } yield (outside, inside)
    )
    lastOutside <- """\w+""".r
  } yield IP(list.map(_._2), list.map(_._1) :+ lastOutside)

  private def abbaOrAba(value: String) = value.charAt(0) != value.charAt(1) && value.reverse == value

  private def hasAbba(value: String) = value.sliding(4).exists(abbaOrAba)

  private def hasAbaReverse(aba: String)(value: String) =
    value.sliding(3).contains(List(aba.charAt(1), aba.charAt(0), aba.charAt(1)).mkString)

  private def getAba(value: String) = value.sliding(3).filter(abbaOrAba).toList

  override def resolveFirst(input: List[String]): Int =
    input.map(parse(ipParser, _).get).count(ip => ip.outside.exists(hasAbba) && !ip.inside.exists(hasAbba))

  override def resolveSecond(input: List[String]): Int =
    input.map(parse(ipParser, _).get).count(ip =>
      ip.outside.flatMap(getAba).exists(aba => ip.inside.exists(hasAbaReverse(aba)))
    )
}
