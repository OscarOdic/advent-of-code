package com.oodic.aoc

import scala.io.Source
import scala.util.parsing.combinator.RegexParsers

object Day12 extends RegexParsers {
  val input = Source.fromResource("day12.txt").getLines.toList

  val communicationParser: Parser[Map[Int, List[Int]]] = for {
    program <- """\d+""".r ^^ (_.toInt)
    _ <- """<->""".r
    programs <- rep1sep("""\d+""".r ^^ (_.toInt), ", ")
  } yield Map(program -> programs)

  def getGroup(origin: Int, communications: Map[Int, List[Int]]) = {
    def rec(contains: List[Int] = List(origin), communicate: List[Int] = List(origin)): List[Int] = {
      communicate.flatMap(communications).distinct.diff(contains) match {
        case Nil => contains
        case l => rec(contains.union(l), l)
      }
    }
    rec()
  }

  def resolveFirst(input: List[String]) = {
    val communications = input.map(parse(communicationParser, _).get).reduce(_ ++ _)
    getGroup(0, communications).size
  }

  def resolveSecond(input: List[String]) = {
    val communications = input.map(parse(communicationParser, _).get).reduce(_ ++ _)
    def rec(keys: List[Int] = communications.keys.toList, n: Int = 0): Int = keys match {
      case Nil => n
      case h::t => rec(keys.diff(getGroup(h, communications)), n+1)
    }
    rec()
  }

  def main(args: Array[String]): Unit = {
    println(s"[first star] ${resolveFirst(input)}")
    println(s"[second star] ${resolveSecond(input)}")
  }
}
