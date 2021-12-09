package com.oodic.aoc.y2017

import scala.util.parsing.combinator.RegexParsers

object Day25 extends Puzzle2017[String, Int, String] with RegexParsers {
  override val input: String = getInputFile.mkString("\n")

  trait Direction
  object Left extends Direction
  object Right extends Direction

  case class State(name: String, one: (Int, Direction, String), two: (Int, Direction, String))

  case class BluePrint(begin: String, steps: Int, states: Map[String, State])

  val stateParser: Parser[State] = for {
    name <- "In state" ~> """\w""".r <~ ":"
    _ <- "If the current value is 0:".r
    value0 <- "- Write the value" ~> """\d""".r <~ "." ^^ (_.toInt)
    direction0 <- "- Move one slot to the" ~> """(left|right)""".r <~ "."
    state0 <- "- Continue with state" ~> """\w""".r <~ "."
    _ <- "If the current value is 1:".r
    value1 <- "- Write the value" ~> """\d""".r <~ "." ^^ (_.toInt)
    direction1 <- "- Move one slot to the" ~> """(left|right)""".r <~ "."
    state1 <- "- Continue with state" ~> """\w""".r <~ "."
  } yield State(
    name,
    (value0, if (direction0 == "right") Right else Left, state0),
    (value1, if (direction1 == "right") Right else Left, state1)
  )

  val bluePrintParser: Parser[BluePrint] = for {
    begin <- "Begin in state" ~> """\w""".r <~ "."
    steps <- "Perform a diagnostic checksum after" ~> """\d+""".r <~ "steps." ^^ (_.toInt)
    states <- rep1(stateParser)
  } yield BluePrint(begin, steps, states.map(s => (s.name, s)).toMap)

  private def executeBluePrint(bluePrint: BluePrint) = {
    def rec(pos: Int = 0, state: String = bluePrint.begin, values: Map[Int, Int] = Map.empty, steps: Int = bluePrint.steps): Map[Int, Int] =
      if (steps == 0) values
      else {
        val rule = if (values.getOrElse(pos, 0) == 0) bluePrint.states(state).one else bluePrint.states(state).two
        rec(
          if (rule._2 == Left) pos - 1 else pos + 1,
          rule._3,
          values.updated(pos, rule._1),
          steps - 1
        )
      }
    rec()
  }

  override def part1(input: String): Int =
    executeBluePrint(parse(bluePrintParser, input).get).values.sum

  override def part2(input: String): String = "No 2nd star"
}
