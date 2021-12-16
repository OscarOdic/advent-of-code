package com.oodic.aoc.y2021

import scala.util.parsing.combinator.RegexParsers

object Day16 extends Puzzle2021[String, Long, Long] with RegexParsers {
  override val input: String =
    getInputFile.head

  trait Packet {
    val version: String
    val typeId: String
    def eval: Long
  }

  case class LiteralPacket(version: String, typeId: String, value: String) extends Packet {
    override def eval: Long = binaryToLong(value)
  }

  case class OperatorPacket(version: String, typeId: String, value: List[Packet]) extends Packet {
    override def eval: Long = typeId match {
      case "000" =>
        value.map(_.eval).sum
      case "001" =>
        value.map(_.eval).product
      case "010" =>
        value.map(_.eval).min
      case "011" =>
        value.map(_.eval).max
      case "101" =>
        value.map(_.eval) match {
          case List(left, right) if left > right => 1
          case _ => 0
        }
      case "110" =>
        value.map(_.eval) match {
          case List(left, right) if left < right => 1
          case _ => 0
        }
      case "111" =>
        value.map(_.eval) match {
          case List(left, right) if left == right => 1
          case _ => 0
        }
    }
  }

  private def binaryToLong(binary: String): Long =
    BigInt(binary, 2).toLong

  private def parseLiteralPacket = for {
    version <- """[01]{3}""".r
    typeId <- "100".r
    values <- rep("""1[01]{4}""".r ^^ (_.tail))
    lastValue <- """0[01]{4}""".r ^^ (_.tail)
  } yield LiteralPacket(version, typeId, s"${values.mkString}$lastValue")

  private def parseOperatorLengthPacket = for {
    version <- """[01]{3}""".r
    typeId <- "[01]{3}".r
    if typeId != "100"
    _ <- "0".r
    length <- """[01]{15}""".r ^^ binaryToLong
    subPackets <- (s"""[01]{$length}""").r ^^ (parse(rep(parseParquet), _).get)
  } yield OperatorPacket(version, typeId, subPackets)

  private def parseOperatorNPacket = for {
    version <- """[01]{3}""".r
    typeId <- "[01]{3}".r
    if typeId != "100"
    _ <- "1".r
    number <- """[01]{11}""".r ^^ (binaryToLong(_).toInt)
    subPackets <- repN(number, parseParquet)
  } yield OperatorPacket(version, typeId, subPackets)

  private val parseParquet: Parser[Packet] = parseLiteralPacket | parseOperatorLengthPacket | parseOperatorNPacket

  private def versionSum(packet: Packet): Long =
    packet match {
      case LiteralPacket(version, _, _) => binaryToLong(version)
      case OperatorPacket(version, _, value) => value.map(versionSum).sum + binaryToLong(version)
    }

  private def hexToBinary(input: String) =
    input.toList.map {
      case '0' => "0000"
      case '1' => "0001"
      case '2' => "0010"
      case '3' => "0011"
      case '4' => "0100"
      case '5' => "0101"
      case '6' => "0110"
      case '7' => "0111"
      case '8' => "1000"
      case '9' => "1001"
      case 'A' => "1010"
      case 'B' => "1011"
      case 'C' => "1100"
      case 'D' => "1101"
      case 'E' => "1110"
      case 'F' => "1111"
    }.mkString

  override def part1(input: String): Long =
    versionSum(parse(parseParquet, hexToBinary(input)).get)

  override def part2(input: String): Long = {
    parse(parseParquet, hexToBinary(input)).get.eval
  }
}