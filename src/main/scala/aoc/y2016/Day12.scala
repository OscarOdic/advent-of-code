package aoc.y2016

import scala.util.parsing.combinator.RegexParsers

object Day12 extends Puzzle2016[List[String], Int, Int] with RegexParsers {
  override val input: List[String] = getInputFile

  trait Instruction
  case class Cpy(value: Either[Int, Char], register: Char) extends Instruction
  case class Inc(register: Char) extends Instruction
  case class Dec(register: Char) extends Instruction
  case class Jnz(value: Either[Int, Char], jump: Int) extends Instruction

  private val parseRegister = """\D""".r ^^ (_.head)

  private val parseValue = """\d+""".r ^^ (x => Left(x.toInt)) | parseRegister ^^ (Right(_))

  private val parseCpy: Parser[Cpy] = for {
    value <- "cpy" ~> parseValue
    register <- parseRegister
  } yield Cpy(value, register)

  private val parseInc: Parser[Inc] = for {
    register <- "inc" ~> parseRegister
  } yield Inc(register)

  private val parseDec: Parser[Dec] = for {
    register <- "dec" ~> parseRegister
  } yield Dec(register)

  private val parseJnz: Parser[Jnz] = for {
    value <- "jnz" ~> parseValue
    jump <- """-?\d+""".r ^^ (_.toInt)
  } yield Jnz(value, jump)

  private val parseInstruction: Parser[Instruction] = parseCpy | parseInc | parseDec | parseJnz

  private def getValue(value: Either[Int, Char], registers: Map[Char, Int]) = value match {
    case Left(number) => number
    case Right(register) => registers.getOrElse(register, 0)
  }

  private def execute(
                       instructions: Vector[Instruction],
                       registers: Map[Char, Int] = Map(),
                       i: Int = 0
                     ): Map[Char, Int] =
    if (i >= instructions.length) registers
    else instructions(i) match {
      case Cpy(value, register) => execute(instructions, registers.updated(register, getValue(value, registers)), i+1)
      case Inc(register) => execute(instructions, registers.updated(register, registers.getOrElse(register, 0) + 1), i+1)
      case Dec(register) => execute(instructions, registers.updated(register, registers.getOrElse(register, 0) - 1), i+1)
      case Jnz(value, jump) =>
        if (getValue(value, registers) != 0) execute(instructions, registers, i+jump)
        else execute(instructions, registers, i+1)
    }

  override def part1(input: List[String]): Int =
    execute(input.map(parse(parseInstruction, _).get).toVector).getOrElse('a', 0)

  override def part2(input: List[String]): Int =
    execute(input.map(parse(parseInstruction, _).get).toVector, Map('c' -> 1)).getOrElse('a', 0)
}