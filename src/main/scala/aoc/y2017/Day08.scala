package aoc.y2017

import scala.util.parsing.combinator.RegexParsers

object Day08 extends Puzzle2017[List[String], Int, Int] with RegexParsers {
  override val input: List[String] = getInputFile

  val Inc = "inc"
  val Dec = "dec"
  val Sup = ">"
  val SupEq = ">="
  val Inf = "<"
  val InfEq = "<="
  val Eq = "=="
  val NotEq = "!="

  case class Operation(register: String, op: String, value: Int) {
    def eval(registers: Map[String, Int]): Map[String, Int] = {
      val registerValue = registers.getOrElse(register, 0)
      registers.updated(
        register,
        op match {
          case Inc => registerValue + value
          case Dec => registerValue - value
        }
      )
    }
  }

  case class Condition(register: String, cond: String, value: Int) {
    def eval(registers: Map[String, Int]): Boolean = {
      val registerValue = registers.getOrElse(register, 0)
      cond match {
        case Sup => registerValue > value
        case SupEq => registerValue >= value
        case Inf => registerValue < value
        case InfEq => registerValue <= value
        case Eq => registerValue == value
        case NotEq => registerValue != value
      }
    }
  }

  case class Instruction(operation: Operation, condition: Condition) {
    def eval(registers: Map[String, Int]): Map[String, Int] =
      if (condition.eval(registers))
        operation.eval(registers)
      else
        registers
  }

  val operationParser: Parser[Operation] = for {
    register <- """[a-z]+""".r
    op <- Inc.r | Dec.r
    value <- """-?\d+""".r ^^ (_.toInt)
  } yield Operation(register, op, value)

  val conditionParser: Parser[Condition] = for {
    register <- """[a-z]+""".r
    cond <- SupEq.r | InfEq.r | Sup.r | Inf.r | Eq.r | NotEq.r
    value <- """-?\d+""".r ^^ (_.toInt)
  } yield Condition(register, cond, value)

  val instructionParser: Parser[Instruction] = for {
    operation <- operationParser
    _ <- "if".r
    condition <- conditionParser
  } yield Instruction(operation, condition)

  override def part1(instructions: List[String]): Int = instructions
    .map(parse(instructionParser, _).get)
    .foldLeft(Map[String, Int]())((registers, instruction) => instruction.eval(registers))
    .maxBy(_._2)
    ._2

  override def part2(instructions: List[String]): Int = instructions
    .map(parse(instructionParser, _).get)
    .foldLeft((Map[String, Int](), 0)) {
      case ((registers, max), instruction) =>
        val newRegisters = instruction.eval(registers)
        val newValue = newRegisters.getOrElse(instruction.operation.register, 0)
        (newRegisters, if (newValue > max) newValue else max)
    }._2
}
