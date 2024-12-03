package aoc.y2024

import scala.util.parsing.combinator.RegexParsers

object Day03 extends Puzzle2024[List[String], Int, Int] with RegexParsers {
  override val input: List[String] = getInputFile

  private trait Instruction
  private case class Mul(left: Int, right: Int) extends Instruction
  private object Do extends Instruction
  private object Dont extends Instruction

  private def cleanInput(value: String, containDo: Boolean = false): String =
    val regexValue = if (containDo) """(mul\((\d+),(\d+)\))|(don't\(\))|(do\(\))""".r else """mul\((\d+),(\d+)\)""".r
    regexValue.findAllMatchIn(value).toList.map(_.toString).mkString

  private def mulParser: Parser[Mul] =
    for {
      left <- "mul(" ~> """\d+""".r <~ "," ^^ (_.toInt)
      right <- """\d+""".r <~ ")" ^^ (_.toInt)
    } yield Mul(left, right)

  private def instructionsParser: Parser[List[Instruction]] = rep(for {
    instruction <- mulParser | ("""do\(\)""".r ^^ (_ => Do)) | ("""don't\(\)""".r ^^ (_ => Dont))
  } yield instruction)

  private def executeInstructions(instructions: List[Instruction], active: Boolean = true): (Boolean, Int) =
    instructions.foldLeft((active, 0)) {
      case ((active, acc), instruction) =>
        instruction match
          case Do => (true, acc)
          case Dont => (false, acc)
          case Mul(left, right) =>
            if active then
              (active, acc + (left * right))
            else
              (active, acc)
    }

  override def part1(input: List[String]): Int =
    input
      .map(x => cleanInput(x))
      .map(parse(instructionsParser, _).get)
      .foldLeft(0)((acc, instructions) => acc + executeInstructions(instructions)._2)

  override def part2(input: List[String]): Int =
    input
      .map(x => cleanInput(x, true))
      .map(parse(instructionsParser, _).get)
      .foldLeft((true, 0)) {
        case ((active, acc), instructions) =>
          val result = executeInstructions(instructions, active)
          (result._1, result._2 + acc)
      }._2
}
