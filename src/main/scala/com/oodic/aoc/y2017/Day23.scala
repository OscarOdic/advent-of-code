package com.oodic.aoc.y2017

import scala.util.parsing.combinator.RegexParsers

object Day23 extends Puzzle2017[Vector[String], Int, Int] with RegexParsers {
  override val input: Vector[String] = getInputFile.toVector

  trait Instruction
  case class Set(register: String, value: String) extends Instruction
  case class Sub(register: String, value: String) extends Instruction
  case class Mul(register: String, value: String) extends Instruction
  case class Jnz(register: String, value: String) extends Instruction

  case class Program(
                      registers: Map[String, Long] = Map.empty,
                      nbMul: Int = 0,
                      index: Int = 0
                    ) {
    def next(instructions: Vector[Instruction]): Program = instructions(index) match {
      case Set(register, value) => Program(
        registers.updated(register, get(value, registers)),
        nbMul,
        index + 1
      )
      case Sub(register, value) => Program(
        registers.updated(register, getRegisterValue(register, registers) - get(value, registers)),
        nbMul,
        index + 1
      )
      case Mul(register, value) => Program(
        registers.updated(register, getRegisterValue(register, registers) * get(value, registers)),
        nbMul + 1,
        index + 1
      )
      case Jnz(register, value) => Program(
        registers,
        nbMul,
        if (get(register, registers) != 0) index + get(value, registers).toInt else index + 1
      )
    }
  }

  val setParser: Parser[Set] = for {
    register <- "set" ~> """\w""".r
    value <- """-?\w+""".r
  } yield Set(register, value)

  val subParser: Parser[Sub] = for {
    register <- "sub" ~> """\w""".r
    value <- """-?\w+""".r
  } yield Sub(register, value)

  val mulParser: Parser[Mul] = for {
    register <- "mul" ~> """\w""".r
    value <- """-?\w+""".r
  } yield Mul(register, value)

  val jnzParser: Parser[Jnz] = for {
    register <- "jnz" ~> """\w""".r
    value <- """-?\w+""".r
  } yield Jnz(register, value)

  val instructionParser: Parser[Instruction] =
    setParser | subParser | mulParser |  jnzParser

  private def get(value: String, registers: Map[String, Long]): Long =
    parse("""[a-z]""".r, value).map(registers.getOrElse(_, 0L)).getOrElse(value.toLong)

  private def getRegisterValue(register: String, registers: Map[String, Long]): Long =
    registers.getOrElse(register, 0)

  private def execute(program: Program, instructions: Vector[Instruction]): Program =
    if (program.index < 0 || program.index >= instructions.size) program
    else execute(program.next(instructions), instructions)

  override def part1(input: Vector[String]): Int =
    execute(Program(), input.map(parse(instructionParser, _).get)).nbMul

  override def part2(input: Vector[String]): Int = {
    val instructions = input.map(_.split(" "))
    val b = instructions(0)(2).toInt * instructions(4)(2).toInt - instructions(5)(2).toInt
    val c = b - instructions(7)(2).toInt
    val jmp = math.abs(instructions(30)(2).toInt)

    (b to c by jmp).foldLeft(0)((h, value) =>
      if ((2 until value).exists(value % _ == 0)) h + 1
      else h
    )
  }
}
