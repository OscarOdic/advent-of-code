package com.oodic.aoc.y2020

import scala.annotation.tailrec
import scala.util.parsing.combinator.RegexParsers

object Day14 extends Puzzle2020[List[String], BigInt, BigInt] with RegexParsers {
  override val input: List[String] =
    getInputFile

  trait Instruction
  case class Mask(value: String) extends Instruction
  case class Write(mem: Int, value: Int) extends Instruction

  private val parseMask =
    "mask =" ~> """.+""".r ^^ Mask

  private val parseWrite = for {
    mem <- "mem[" ~> """\d+""".r <~ "]" ^^ (_.toInt)
    value <- "=" ~> """\d+""".r ^^ (_.toInt)
  } yield Write(mem, value)

  private val parseInstruction = parseMask | parseWrite

  private def completeZeros(s: String): String =
    List.fill(36 - s.length)('0').mkString + s

  private def version1(write: Write, memory: Map[BigInt, BigInt], mask: String): Map[BigInt, BigInt] =
    write match {
      case Write(mem, value) =>
        memory.updated(mem,
          BigInt((completeZeros(value.toBinaryString).zip(mask) map {
            case (_, '0') => '0'
            case (_, '1') => '1'
            case (c, _) => c
          }).mkString, 2))
    }

  private def version2(write: Write, memory: Map[BigInt, BigInt], mask: String): Map[BigInt, BigInt] =
    write match {
      case Write(mem, value) =>
        val result = completeZeros(mem.toBinaryString).zip(mask) map {
          case (c, '0') => c
          case (_, '1') => '1'
          case _ => 'X'
        }
        result
          .zipWithIndex
          .filter(_._1 == 'X').map(_._2)
          .foldLeft(List(result)) ((memories, index) =>
            memories.flatMap(mem =>
              List(mem.updated(index, '0'), mem.updated(index, '1'))
            )
          ).map(l => BigInt(l.mkString, 2))
          .foldLeft(memory)((memory, indexMemory) => memory.updated(indexMemory, value))
    }

  @tailrec
  private def execute(instructions: List[Instruction], memory: Map[BigInt, BigInt] = Map.empty, mask: String = "")
                     (version: (Write, Map[BigInt, BigInt], String) => Map[BigInt, BigInt])
  : BigInt =
    instructions match {
      case Mask(value) :: nextInstructions =>
        execute(nextInstructions, memory, value)(version)
      case (write: Write) :: nextInstructions =>
        execute(nextInstructions, version(write, memory, mask), mask)(version)
      case _ =>
        memory.values.sum
    }

  override def part1(input: List[String]): BigInt =
    execute(input.map(parse(parseInstruction, _).get))(version1)

  override def part2(input: List[String]): BigInt =
    execute(input.map(parse(parseInstruction, _).get))(version2)
}
