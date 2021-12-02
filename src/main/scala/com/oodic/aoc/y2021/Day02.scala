package com.oodic.aoc.y2021

import scala.util.parsing.combinator.RegexParsers

object Day02 extends Puzzle2021[List[String], Int, Int] with RegexParsers{
  override val input: List[String] =
    getInputFile

  trait Move
  case class Forward(x: Int) extends Move
  case class Up(x: Int) extends Move
  case class Down(x: Int) extends Move

  private val forwardParser = for {
    x <- "forward" ~> """\d+""".r ^^ (_.toInt)
  } yield Forward(x)

  private val upParser = for {
    x <- "up" ~> """\d+""".r ^^ (_.toInt)
  } yield Up(x)

  private val downParser = for {
    x <- "down" ~> """\d+""".r ^^ (_.toInt)
  } yield Down(x)

  private val moveParser = forwardParser | upParser | downParser

  override def resolveFirst(input: List[String]): Int =
    input.map(parse(moveParser, _).get).foldLeft((0, 0)) {
      case ((horizon, depth), Forward(x)) => (horizon + x, depth)
      case ((horizon, depth), Up(x)) => (horizon, depth - x)
      case ((horizon, depth), Down(x)) => (horizon, depth + x)
    } match {
      case (horizon, depth) => horizon * depth
    }

  override def resolveSecond(input: List[String]): Int =
    input.map(parse(moveParser, _).get).foldLeft(((0, 0), 0)) {
      case (((horizon, depth), aim), Forward(x)) => ((horizon + x, depth + (aim * x)), aim)
      case ((pos, aim), Up(x)) => (pos, aim - x)
      case ((pos, aim), Down(x)) => (pos, aim + x)
    } match {
      case ((horizon, depth), _) => horizon * depth
    }
}
