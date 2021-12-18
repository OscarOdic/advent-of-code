package com.oodic.aoc.y2021

import scala.annotation.tailrec
import scala.util.parsing.combinator.RegexParsers

object Day18 extends Puzzle2021[List[String], Int, Int] with RegexParsers {
  override val input: List[String] =
    getInputFile

  trait Value {
    def addValueLeft(v: Int): Value
    def addValueRight(v: Int): Value
  }

  case class RegularNumber(value: Int) extends Value {
    override def addValueLeft(v: Int): Value = RegularNumber(value + v)
    override def addValueRight(v: Int): Value = addValueLeft(v)
  }

  case class Pair(left: Value, right: Value) extends Value {
    override def addValueLeft(v: Int): Value = copy(left = left.addValueLeft(v))
    override def addValueRight(v: Int): Value = copy(right = right.addValueRight(v))
  }

  private val parseRegularNumber = """\d+""".r ^^ (value => RegularNumber(value.toInt))

  private val parsePair = for {
    left <- "[" ~> parseValue
    right <- "," ~> parseValue <~ "]"
  } yield Pair(left, right)

  private val parseValue: Parser[Value] = parseRegularNumber | parsePair

  private def split(value: Value): Option[Value] =
    value match {
      case RegularNumber(v) if v >= 10 =>
        Some(Pair(RegularNumber(v/2), RegularNumber((v/2.0).ceil.toInt)))
      case Pair(left, right) =>
        split(left).map(Pair(_, right)).orElse(
          split(right).map(Pair(left, _)
        ))
      case _ => None
    }

  private def explode(value: Value, depth: Int = 0): Option[(Option[Int], Value, Option[Int])] =
    value match {
      case Pair(RegularNumber(left), RegularNumber(right)) if depth >= 4 =>
        Some((Some(left), RegularNumber(0), Some(right)))
      case Pair(left, right) =>
        explode(left, depth + 1).map {
          case (leftAdd, left, rightAdd) => (leftAdd, Pair(left, rightAdd.map(right.addValueLeft).getOrElse(right)), None)
        }.orElse(explode(right, depth + 1).map {
          case (leftAdd, right, rightAdd) => (None, Pair(leftAdd.map(left.addValueRight).getOrElse(left), right), rightAdd)
        })
      case _ => None
    }

  private def additionsMagnitude(values: List[Value]): Int =
    magnitude(values.tail.foldLeft(execute(values.head))((x, y) => execute(Pair(x, y))))

  private def largestAdditionsMagnitude(values: Seq[Value]): Int =
    (for {
      left <- values.iterator
      right <- values.iterator
    } yield magnitude(execute(Pair(left, right)))).max


  private def magnitude(value: Value): Int =
    value match {
      case RegularNumber(v) => v
      case Pair(left, right) => 3 * magnitude(left) + 2 * magnitude(right)
    }


  private def execute(value: Value): Value =
    explode(value) match {
      case Some((_, result, _)) => execute(result)
      case _ =>
        split(value) match {
          case Some(result) => execute(result)
          case _ => value
        }
    }

  override def part1(input: List[String]): Int =
    additionsMagnitude(input.map(parse(parseValue, _).get))

  override def part2(input: List[String]): Int =
    largestAdditionsMagnitude(input.map(parse(parseValue, _).get))
}
