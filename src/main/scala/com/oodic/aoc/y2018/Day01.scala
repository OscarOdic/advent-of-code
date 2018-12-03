package com.oodic.aoc.y2018

import scala.util.parsing.combinator.RegexParsers

object Day01 extends Puzzle2018[List[String], Int, Int] with RegexParsers {
  override val input: List[String] = getInputFile

  trait Operation {
    def execute(n: Int): Int
  }
  case class Addition(value: Int) extends Operation {
    override def execute(n: Int): Int = n + value
  }
  case class Deletion(value: Int) extends Operation {
    override def execute(n: Int): Int = n - value
  }

  private val additionParser: Parser[Addition] = ("+" ~> """\d+""".r ^^ (_.toInt)).map(Addition)

  private val deletionParser: Parser[Deletion] = ("-" ~> """\d+""".r ^^ (_.toInt)).map(Deletion)

  private val operationParser: Parser[Operation] = additionParser | deletionParser

  override def resolveFirst(input: List[String]): Int = input.map(parse(operationParser, _).get)
    .foldLeft(0)((current, op) => op.execute(current))

  override def resolveSecond(input: List[String]): Int = {
    def rec(current: Int, history: Set[Int], operations: List[Operation]): Int = {
      if (history.contains(current)) current
      else rec(operations.head.execute(current), history + current, operations.tail :+ operations.head)
    }
    rec(0, Set(), input.map(parse(operationParser, _).get))
  }
}
