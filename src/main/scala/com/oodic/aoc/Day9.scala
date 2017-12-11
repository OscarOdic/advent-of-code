package com.oodic.aoc

import scala.io.Source
import scala.util.parsing.combinator.RegexParsers

object Day9 extends RegexParsers {
  val input = Source.fromResource("day9.txt").getLines.mkString

  trait Block {
    def sum = 0
    def charactInGarbage: Int
  }
  case class Garbage(n: Int) extends Block {
    override def charactInGarbage = n
  }
  case class Group(p: Int, blocks: List[Block]) extends Block {
    override def sum = p + blocks.map(_.sum).sum
    override def charactInGarbage: Int = blocks.map(_.charactInGarbage).sum
  }

  def garbageParser: Parser[Garbage] = for {
    list <- '<' ~> rep("""!.""".r ^^ (_ => 0) | """[^>]""".r ^^ (_ => 1)) <~ '>'
  } yield Garbage(list.sum)

  def groupParser(p: Int = 1): Parser[Group] = for {
    blocks <- '{' ~> repsep(groupParser(p+1) | garbageParser, ",") <~ '}'
  } yield Group(p, blocks)

  def resolveFirst(input: String) = parse(groupParser(), input).get.sum

  def resolveSecond(input: String) = parse(groupParser(), input).get.charactInGarbage

  def main(args: Array[String]): Unit = {
    println(s"[first star] ${resolveFirst(input)}")
    println(s"[second star] ${resolveSecond(input)}")
  }
}
