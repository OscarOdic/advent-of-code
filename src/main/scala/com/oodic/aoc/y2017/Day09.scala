package com.oodic.aoc.y2017

import scala.util.parsing.combinator.RegexParsers

object Day09 extends Puzzle2017[String, Int, Int] with RegexParsers {
  override val input: String = getInputFile.mkString

  trait Block {
    def sum = 0
    def charactersInGarbage: Int
  }
  case class Garbage(n: Int) extends Block {
    override def charactersInGarbage: Int = n
  }
  case class Group(p: Int, blocks: List[Block]) extends Block {
    override def sum: Int = p + blocks.map(_.sum).sum
    override def charactersInGarbage: Int = blocks.map(_.charactersInGarbage).sum
  }

  def garbageParser: Parser[Garbage] = for {
    list <- '<' ~> rep("""!.""".r ^^ (_ => 0) | """[^>]""".r ^^ (_ => 1)) <~ '>'
  } yield Garbage(list.sum)

  def groupParser(p: Int = 1): Parser[Group] = for {
    blocks <- '{' ~> repsep(groupParser(p+1) | garbageParser, ",") <~ '}'
  } yield Group(p, blocks)

  override def resolveFirst(input: String): Int = parse(groupParser(), input).get.sum

  override def resolveSecond(input: String): Int = parse(groupParser(), input).get.charactersInGarbage
}
