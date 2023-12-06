package aoc.y2022

import scala.util.parsing.combinator.RegexParsers

object Day10 extends Puzzle2022[List[String], Int, String] with RegexParsers {
  override val input: List[String] = getInputFile

  private trait Operation

  private object Noop extends Operation

  private case class Addx(x: Int) extends Operation

  private def operationParser: Parser[Operation] =
    "noop".r ^^ (_ => Noop) | "addx" ~> """-?\d+""".r ^^ (value => Addx(value.toInt))

  private def parseOperations(operations: List[String]): List[Operation] = 
    operations.map(parse(operationParser, _).get)
      .flatMap {
        case op: Addx => List(Noop, op)
        case Noop => List(Noop)
      }

  private def getRegisterSignals(operations: List[Operation]): List[Int] =
    operations.foldLeft((List(1), 1)) { case ((registerSignals, register), operation) =>
      operation match
        case Noop => (registerSignals :+ register, register)
        case Addx(x) => (registerSignals :+ (register + x), register + x)
    }._1

  private def generateSprite(x: Int): List[Char] =
    (x to x + 2).foldLeft(List.fill(40)('.'))((sprite, pixel) =>
      if (((pixel - 1) >= sprite.length) || (pixel < 1)) sprite
      else {
        sprite.updated(pixel - 1, '#')
      }
    )

  private val defaultSprite: List[Char] = generateSprite(1)

  private def drawPixels(
                          signals: List[Int],
                          drawing: String = "",
                          cycle: Int = 1,
                          sprite: List[Char] = defaultSprite
                        ): String =
    signals match
      case head :: tail =>
        val newSprite = generateSprite(head)
        drawPixels(tail, drawing :+ newSprite((cycle - 1) % 40), cycle + 1, newSprite)
      case _ => drawing

  override def part1(input: List[String]): Int =
    val registerSignals = getRegisterSignals(parseOperations(input))
    List(20, 60, 100, 140, 180, 220)
      .map(cycle => registerSignals(cycle - 1) * cycle)
      .sum

  override def part2(input: List[String]): String =
    val registerSignals = getRegisterSignals(parseOperations(input))
    "\n" ++ drawPixels(registerSignals)
      .take(240)
      .grouped(40)
      .mkString("\n")
}
