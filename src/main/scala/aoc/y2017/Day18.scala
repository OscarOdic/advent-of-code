package aoc.y2017

import scala.collection.immutable.Queue
import scala.util.parsing.combinator.RegexParsers

object Day18 extends Puzzle2017[Vector[String], Long, Int] with RegexParsers {
  override val input: Vector[String] = getInputFile.toVector

  trait Instruction
  case class Snd(register: String) extends Instruction
  case class Set(register: String, value: String) extends Instruction
  case class Add(register: String, value: String) extends Instruction
  case class Mul(register: String, value: String) extends Instruction
  case class Mod(register: String, value: String) extends Instruction
  case class Rcv(register: String) extends Instruction
  case class Jgz(register: String, value: String) extends Instruction

  case class Program(
                      registers: Map[String, Long] = Map.empty,
                      index: Int = 0,
                      nbSend: Int = 0,
                      waiting: Boolean = false,
                      terminated: Boolean = false
                    )

  val sndParser: Parser[Snd] = for {
    register <- "snd" ~> """-?\w""".r
  } yield Snd(register)

  val setParser: Parser[Set] = for {
    register <- "set" ~> """\w""".r
    value <- """-?\w+""".r
  } yield Set(register, value)

  val addParser: Parser[Add] = for {
    register <- "add" ~> """\w""".r
    value <- """-?\w+""".r
  } yield Add(register, value)

  val mulParser: Parser[Mul] = for {
    register <- "mul" ~> """\w""".r
    value <- """-?\w+""".r
  } yield Mul(register, value)

  val modParser: Parser[Mod] = for {
    register <- "mod" ~> """\w""".r
    value <- """-?\w+""".r
  } yield Mod(register, value)

  val rcvParser: Parser[Rcv] = for {
    register <- "rcv" ~> """\w""".r
  } yield Rcv(register)

  val jgzParser: Parser[Jgz] = for {
    register <- "jgz" ~> """\w""".r
    value <- """-?\w+""".r
  } yield Jgz(register, value)

  val instructionParser: Parser[Instruction] =
    sndParser | setParser | addParser | mulParser | modParser | rcvParser | jgzParser

  def getInstructionValue(value: String, registers: Map[String, Long]): Long =
    parse("""[a-z]""".r, value).map(registers.getOrElse(_, 0L)).getOrElse(value.toLong)

  def getRegisterValue(register: String, registers: Map[String, Long]): Long =
    registers.getOrElse(register, 0)

  private def execOp(registers: Map[String, Long])(instruction: Instruction) =
    instruction match {
      case Set(register, value) =>
        registers.updated(register, getInstructionValue(value, registers))
      case Add(register, value) =>
        registers.updated(register, getRegisterValue(register, registers) + getInstructionValue(value, registers))
      case Mul(register, value) =>
        registers.updated(register, getRegisterValue(register, registers) * getInstructionValue(value, registers))
      case Mod(register, value) =>
        registers.updated(register, getRegisterValue(register, registers) % getInstructionValue(value, registers))
      case _ =>
        registers
    }

  private def execProgram(program: Program, queueIn: Queue[Long], queueOut: Queue[Long], instructions: Vector[Instruction], otherProg: Program): (Program, Queue[Long], Queue[Long]) =
    if (program.index < 0 || program.index >= instructions.size) {
      (program.copy(terminated = true), queueOut, queueIn)
    }
    else {
      instructions(program.index) match {
        case Snd(register) =>
          execProgram(
            program.copy(
              index = program.index + 1,
              nbSend = program.nbSend + 1
            ),
            queueIn,
            queueOut.enqueue(getRegisterValue(register, program.registers)),
            instructions,
            otherProg
          )
        case Rcv(register) =>
          if (queueIn.isEmpty) {
            if (otherProg.terminated || (queueOut.isEmpty && otherProg.waiting))
              (program.copy(terminated = true), queueIn, queueOut)
            else
              (program.copy(waiting = true), queueIn, queueOut)
          }
          else {
            val (value, newQueue) = queueIn.dequeue
            execProgram(
              program.copy(
                registers = program.registers.updated(register, value),
                index = program.index + 1,
                waiting = false
              ),
              newQueue,
              queueOut,
              instructions,
              otherProg
            )
          }
        case Jgz(register, value) =>
          val instructionValue = getInstructionValue(value, program.registers)
          if (getInstructionValue(register, program.registers) > 0L) {
            execProgram(
              program.copy(
                index = program.index + instructionValue.toInt
              ),
              queueIn,
              queueOut,
              instructions,
              otherProg
            )
          }
          else (program.copy(index = program.index + 1), queueIn, queueOut)
        case instruction =>
          val newRegisters = execOp(program.registers)(instruction)
          execProgram(
            program.copy(
              registers = newRegisters,
              index = program.index + 1
            ),
            queueIn,
            queueOut,
            instructions,
            otherProg
          )
      }
    }

  override def part1(input: Vector[String]): Long = {
    val instructions = input.map(parse(instructionParser, _).get)
    def rec(registers: Map[String, Long] = Map.empty, history: Map[String, Long] = Map.empty, index: Int = 0): Long = instructions(index) match {
      case Snd(register) =>
        rec(registers, history.updated(register, getRegisterValue(register, registers)), index + 1)
      case Rcv(register) if getRegisterValue(register, registers) != 0 =>
        history.getOrElse(register, rec(registers, history, index + 1))
      case Jgz(register, value) if getRegisterValue(register, registers) != 0 =>
        rec(registers, history, index + getInstructionValue(value, registers).toInt)
      case instruction =>
        val newRegisters = execOp(registers)(instruction)
        rec(newRegisters, history, index + 1)
    }
    rec()
  }

  override def part2(input: Vector[String]): Int = {
    val instructions = input.map(parse(instructionParser, _).get)
    def rec(
             firstProgram: Program = Program().copy(registers = Map("p" -> 0)),
             secondProgram: Program = Program().copy(registers = Map("p" -> 1)),
             queueFirstToSecond: Queue[Long] = Queue.empty,
             queueSecondToFirst: Queue[Long] = Queue.empty
           ): Int = {
      val (newFirstProgram, newQueueSecondToFirst, newQueueFirstToSecond) = execProgram(firstProgram, queueSecondToFirst, queueFirstToSecond, instructions, secondProgram)
      val (newSecondProgram, finalQueueFirstToSecond, finalQueueSecondToFirst) = execProgram(secondProgram, newQueueFirstToSecond, newQueueSecondToFirst, instructions, newFirstProgram)

      if (newFirstProgram.terminated || newSecondProgram.terminated || (newFirstProgram.waiting && newSecondProgram.waiting))
        newSecondProgram.nbSend
      else
        rec(
          newFirstProgram,
          newSecondProgram,
          finalQueueFirstToSecond,
          finalQueueSecondToFirst
        )
    }
    rec()
  }
}
