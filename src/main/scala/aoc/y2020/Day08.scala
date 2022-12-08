package aoc.y2020

import scala.annotation.tailrec
import scala.util.parsing.combinator.RegexParsers

object Day08 extends Puzzle2020[Vector[String], Int, Int] with RegexParsers {
  override val input: Vector[String] =
    getInputFile.toVector

  trait Instruction
  case class Acc(value: Int) extends Instruction
  case class Jmp(value: Int) extends Instruction
  case class Nop(value: Int) extends Instruction

  private val accParser = "acc" ~> """([+\-])\d+""".r ^^ (value => Acc(value.toInt))

  private val jmpParser = "jmp" ~> """([+\-])\d+""".r ^^ (value => Jmp(value.toInt))

  private val nopParser = "nop" ~> """([+\-])\d+""".r ^^ (value => Nop(value.toInt))

  private val instructionParser = accParser | jmpParser | nopParser

  private def execute(instruction: Instruction, index: Int, accumulator: Int): (Int, Int) =
    instruction match {
      case Acc(value) => (index + 1, accumulator + value)
      case Jmp(value) => (index + value, accumulator)
      case Nop(_) => (index + 1, accumulator)
    }

  @tailrec
  private def executeWithoutLoop(instructions: Vector[(Instruction, Boolean)], index: Int = 0, accumulator: Int = 0): (Int, Boolean) = {
    if (index >= instructions.size)
      (accumulator, true)
    else
      instructions.apply(index) match {
        case (instruction, false) =>
          val (nextIndex, nextAccumulator) = execute(instruction, index, accumulator)
          executeWithoutLoop(instructions.updated(index, (instruction, true)), nextIndex, nextAccumulator)
        case _ =>
          (accumulator, false)
    }
  }

  private def swapInstructions(instructions: Vector[Instruction], index: Int): Vector[Instruction] =
    instructions
      .updated(index,
        instructions(index) match {
          case Jmp(value) => Nop(value)
          case Nop(value) => Jmp(value)
        }
      )

  private def swapIndexPossibilities(instructions: Vector[Instruction]): List[Int] =
    instructions
      .zipWithIndex
      .filter(!_._1.isInstanceOf[Acc])
      .map(_._2)
      .toList

  @tailrec
  private def executeUncorrupted(instructions: Vector[Instruction], swapIndex: List[Int]): Int =
    swapIndex match {
      case i :: t =>
        executeWithoutLoop(swapInstructions(instructions, i).map((_, false))) match {
          case (acc, true) => acc
          case _ => executeUncorrupted(instructions, t)
        }
      case _ => 0
    }


  override def part1(input: Vector[String]): Int =
    executeWithoutLoop(input
      .map(instruction => (parse(instructionParser, instruction).get, false))
    )._1

  override def part2(input: Vector[String]): Int = {
    val instructions = input
      .map(instruction => parse(instructionParser, instruction).get)
    executeUncorrupted(instructions, swapIndexPossibilities(instructions))
  }
}
