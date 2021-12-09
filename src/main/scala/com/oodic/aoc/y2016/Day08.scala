package com.oodic.aoc.y2016

import scala.util.parsing.combinator.RegexParsers

object Day08 extends Puzzle2016[List[String], Int, String] with RegexParsers {
  override val input: List[String] = getInputFile

  trait Direction
  object Column extends Direction
  object Row extends Direction

  trait Instruction
  case class Rect(a: Int, b: Int) extends Instruction
  case class Rotate(direction: Direction, value: Int, by: Int) extends Instruction

  private val rectParser: Parser[Rect] = for {
    a <- "rect" ~> """\d+""".r ^^ (_.toInt)
    b <- "x" ~> """\d+""".r ^^ (_.toInt)
  } yield Rect(a, b)

  private val rotateParser: Parser[Rotate] = for {
    direction <- "rotate" ~> ("column" | "row") ^^ {
      case "column" => Column
      case _ => Row
    }
    value <- (if (direction == Column) "x=" else "y=") ~> """\d+""".r ^^ (_.toInt)
    by <- "by" ~> """\d+""".r ^^ (_.toInt)
  } yield Rotate(direction, value, by)

  private val instructionParser = rectParser | rotateParser

  private def executeRect(rect: Rect, screen: Vector[Vector[Boolean]]) =
    screen.zipWithIndex map {
      case (line, y) if y < rect.b =>
        line.zipWithIndex.map {
          case (_, x) if x < rect.a => true
          case (value, _) => value
        }
      case (line, _) => line
    }

  private def executeRotate(rotate: Rotate, screen: Vector[Vector[Boolean]]) = {
    val screenT = rotate.direction match {
      case Column => screen.transpose
      case Row => screen
    }

    val newScreen = screenT.updated(
      rotate.value,
      screenT(rotate.value).zipWithIndex map {
        case (_, x) =>
          val tmp = (x - rotate.by) % screenT(rotate.value).length
          val newValue = if (tmp >= 0) tmp else tmp + screenT(rotate.value).length
          screenT(rotate.value)(newValue)
      }
    )

    rotate.direction match {
      case Column => newScreen.transpose
      case Row => newScreen
    }
  }

  private def executeInstructions(instructions: List[Instruction], screen: Vector[Vector[Boolean]]) =
    instructions.foldLeft(screen) {
      case (screenAcc, rect: Rect) => executeRect(rect, screenAcc)
      case (screenAcc, rotate: Rotate) => executeRotate(rotate, screenAcc)
    }

  private def resolve(input: List[String]) = executeInstructions(
    input.map(parse(instructionParser, _).get),
    Vector.fill(6)(Vector.fill(50)(false))
  )

  override def part1(input: List[String]): Int =
    resolve(input).map(_.count(identity)).sum

  override def part2(input: List[String]): String =
    "\n" + resolve(input).map(_.map(value => if (value) "#" else ".").mkString).mkString("\n")
}