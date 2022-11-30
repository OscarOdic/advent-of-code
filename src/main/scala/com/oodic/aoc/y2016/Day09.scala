package com.oodic.aoc.y2016

import scala.util.parsing.combinator.RegexParsers

object Day09 extends Puzzle2016[String, Long, Long] with RegexParsers {
  override val input: String = getInputFile.head

  trait Value {
    def simpleDecompressedLength: Long
    def completeDecompressedLength: Long
  }

  case class Simple(characters: String) extends Value {
    def simpleDecompressedLength: Long = characters.length.toLong
    def completeDecompressedLength: Long = simpleDecompressedLength
  }

  case class Compressed(repeat: Int, characters: String) extends Value {
    def simpleDecompressedLength: Long = repeat * characters.length.toLong
    def completeDecompressedLength: Long = repeat * completeDecompressed(characters)
  }

  private val simpleParser: Parser[Simple] = """\w+""".r ^^ Simple.apply

  private val compressedParser: Parser[Compressed] = for {
    number <- "(" ~> """\d+""".r.map(_.toInt)
    repeat <- "x" ~> """\d+""".r.map(_.toInt)
    characters <- ")" ~> (""".{""" + number + """}""").r
  } yield Compressed(repeat, characters)

  private val decompressParser: Parser[List[Value]] = rep(simpleParser | compressedParser)

  private def simpleDecompressed(compressed: String): Long =
    parse(decompressParser, compressed).get.map(_.simpleDecompressedLength).sum

  private def completeDecompressed(compressed: String): Long =
    parse(decompressParser, compressed).get.map(_.completeDecompressedLength).sum

  override def part1(input: String): Long = simpleDecompressed(input)

  override def part2(input: String): Long = completeDecompressed(input)
}