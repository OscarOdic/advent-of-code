package adventOfCode

import scala.io.Source
import scala.util.parsing.combinator.RegexParsers

object Day8 extends RegexParsers {
  val input = Source.fromResource("day8.txt").getLines.toList

  val Inc = "inc"
  val Dec = "dec"
  val Sup = ">"
  val SupEq = ">="
  val Inf = "<"
  val InfEq = "<="
  val Eq = "=="
  val NotEq = "!="

  case class Operation(register: String, op: String, value: Int) {
    def eval(registers: Map[String, Int]) = {
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
    def eval(registers: Map[String, Int]) = {
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
    def eval(registers: Map[String, Int]) = condition.eval(registers) match {
      case true => operation.eval(registers)
      case false => registers
    }
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

  def resolveFirst(instructions: List[String]) = instructions
    .map(parse(instructionParser, _).get)
    .foldLeft(Map[String, Int]())((registers, instruction) => instruction.eval(registers))
    .maxBy(_._2)
    ._2

  def resolveSecond(instructions: List[String]) = instructions
    .map(parse(instructionParser, _).get)
    .foldLeft((Map[String, Int](), 0)) {
      case ((registers, max), instruction) =>
        val newRegisters = instruction.eval(registers)
        val newValue = newRegisters.getOrElse(instruction.operation.register, 0)
        (newRegisters, if (newValue > max) newValue else max)
    }._2

  def main(args: Array[String]): Unit = {
    println(s"[first star] ${resolveFirst(input)}")
    println(s"[second star] ${resolveSecond(input)}")
  }
}
