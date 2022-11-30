package com.oodic.aoc.y2016

import scala.util.parsing.combinator.RegexParsers

object Day21 extends Puzzle2016[List[String], String, String] with RegexParsers {
  override val input: List[String] = getInputFile

  trait Instruction {
    def execute(value: String): String
    def reverse: Instruction
  }

  case class SwapPositions(left: Int, right: Int) extends Instruction {
    override def execute(value: String): String =
      value
        .updated(left, value.charAt(right))
        .updated(right, value.charAt(left))
    override def reverse: SwapPositions = this
  }

  case class SwapLetters(left: String, right: String) extends Instruction {
    override def execute(value: String): String =
      SwapPositions(value.indexOf(left.head), value.indexOf(right.head)).execute(value)
    override def reverse: SwapLetters = this
  }

  case class RotateDirection(direction: String, steps: Int) extends Instruction {
    override def execute(value: String): String = {
      val size = value.length
      direction match {
        case "left" => value.drop(steps % size) ++ value.take(steps % size)
        case _ => value.drop(size - (steps % size)) ++ value.take(size - (steps % size))
      }
    }
    override def reverse: RotateDirection = RotateDirection(if (direction=="left") "right" else "left", steps)
  }

  case class RotatePosition(letter: String) extends Instruction {
    override def execute(value: String): String = {
      val index = value.zipWithIndex.find(_._1 == letter.head).get._2
      RotateDirection("right", index + (if (index >= 4) 2 else 1)).execute(value)
    }
    override def reverse: ReverseRotatePosition = ReverseRotatePosition(letter)
  }

  case class ReverseRotatePosition(letter: String) extends Instruction {
    override def execute(value: String): String =
      value.indices.map(RotateDirection("left", _))
        .map(_.execute(value))
        .find(value == RotatePosition(letter).execute(_)).get
    override def reverse: ReverseRotatePosition = this
  }

  case class Reverse(left: Int, right: Int) extends Instruction {
    override def execute(value: String): String =
      value.take(left) + value.slice(left, right+1).reverse + value.drop(right+1)
    override def reverse: Reverse = this
  }

  case class Move(left: Int, right: Int) extends Instruction {
    override def execute(value: String): String = {
      val newValue = value.take(left) + value.drop(left+1)
      newValue.take(right) + value.charAt(left).toString + newValue.drop(right)
    }
    override def reverse: Move = Move(right, left)
  }

  private val swapPositionsParser: Parser[SwapPositions] = for {
    left <- "swap position" ~> """\d+""".r ^^ (_.toInt)
    right <- "with position" ~> """\d+""".r ^^ (_.toInt)
  } yield SwapPositions(left, right)

  private val swapLettersParser: Parser[SwapLetters] = for {
    left <- "swap letter" ~> """\w""".r
    right <- "with letter" ~> """\w""".r
  } yield SwapLetters(left, right)

  private val rotateDirectionParser: Parser[RotateDirection] = for {
    direction <- "rotate" ~> ("left" | "right")
    steps <- """\d+""".r <~ """steps?""".r ^^ (_.toInt)
  } yield RotateDirection(direction, steps)

  private val rotatePositionParser: Parser[RotatePosition] = for {
    letter <- "rotate based on position of letter" ~> """\w""".r
  } yield RotatePosition(letter)

  private val reverseParser: Parser[Reverse] = for {
    left <- "reverse positions" ~> """\d+""".r ^^ (_.toInt)
    right <- "through" ~> """\d+""".r ^^ (_.toInt)
  } yield Reverse(left, right)

  private val moveParser: Parser[Move] = for {
    left <- "move position" ~> """\d+""".r ^^ (_.toInt)
    right <- "to position" ~> """\d+""".r ^^ (_.toInt)
  } yield Move(left, right)

  private val instructionParser: Parser[Instruction] =
    swapPositionsParser | swapLettersParser | rotateDirectionParser | rotatePositionParser | reverseParser | moveParser

  private def execute(instruction: List[Instruction], value: String) =
    instruction.foldLeft(value)((newValue, instruction) => instruction.execute(newValue))

  override def part1(input: List[String]): String =
    execute(input.map(parse(instructionParser, _).get), "abcdefgh")

  override def part2(input: List[String]): String =
    execute(input.map(parse(instructionParser, _).get).reverseMap(_.reverse), "fbgdceah")
}
